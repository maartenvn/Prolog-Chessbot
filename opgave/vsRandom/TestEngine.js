const { spawn } = require("child_process");
const { Readable } = require("stream");
var Chess = require("./chess").Chess;
var Translate = require("./boardTranslate");
var project = "../../src/main.pl";
var swipl = /^win/.test(process.platform)
  ? "C:\\Program Files\\swipl\\bin\\swipl"
  : "swipl";

// Initialise seed with a random number
var seed = Math.floor(Math.random() * 10000);
var seed_init = seed;

// Set seed if the user specified it
function setSeed() {
  // cleanup ...
  var arg2 = process.argv[2];
  if (arg2) {
    seed = parseInt(arg2);
    seed_init = seed;
  }
}
// Next seeded random number this should really be part of the standard Math lib
// Don't use this for important stuff see:
// https://stackoverflow.com/questions/521295/seeding-the-random-number-generator-in-javascript/521323#521323
function random() {
  var x = Math.sin(seed++) * 10000;
  return x - Math.floor(x);
}

// Generate all the allowed boards
function generateBoards(chess) {
  return chess.moves().map((m) => {
    chess.move(m);
    fen = chess.fen();
    chess.undo();
    return fen;
  });
}

function makeMove(chess) {
  var moves = chess.moves();
  if (moves.length > 0) {
    var r = Math.floor(random() * moves.length);
    var m = moves[r];
    chess.move(m);
  }
  return chess;
}

function checkMove(resolve, reject, fens, data, time) {
  fens = fens.map(Translate.keepImportant);

  try {
    const fen = Translate.decode(data);
    if (!fens.includes(fen)) {
      reject({
        msg: `Not a valid move
Valid move example:
${Translate.encode(fens[0])}

ALL valid moves (shortend FEN notation):
${fens.join("\n")}`,
        data: data,
      });
    } else {
      resolve({ fen: fen + " 10 10", time: time }); // fake a full fen
    }
  } catch (e) {
    reject({ msg: `Could not parse (${e})`, data: data });
  }
}

// Run the engine
function run(fen, fens) {
  var p = new Promise(function (resolve, reject) {
    let result = "";
    let err = "";
    let startTime = new Date(); // reset when all input is given
    let engine = spawn(swipl, ["-f", "-q", "-t", "halt(1)", "-O", project], {
      stdio: ["pipe", "pipe", "pipe"],
    });

    // Unlink swipl from this process
    engine.unref();

    // Collect stdout and stderr
    engine.stdout.on("data", (data) => {
      result += data;
    });
    engine.stderr.on("data", (data) => {
      err += data;
    });

    // Handle exists
    engine.on("error", (code) => {
      engine.kill();
      reject({
        msg: "Process ended with an error code ${code}",
        data: "not inspected due to error",
        err: err,
      });
    });

    engine.on("exit", (code, signal) => {
      engine.kill();
      if (code === 0) {
        checkMove(resolve, reject, fens, result, new Date() - startTime);
      } else {
        reject({
          msg:
            code === null
              ? `Process ended by signal ${signal}`
              : `Process ended with a nonzero exit code ${code}`,
          data: result,
          err: err,
        });
      }
    });

    // absolute timeout
    setTimeout(function () {
      engine.kill();
      reject({
        msg: `Process killed by timeout`,
        data: result,
        err: err,
      });
    }, 100 * 1000);

    const stdinStream = new Readable();
    stdinStream.pipe(engine.stdin);
    stdinStream.on("error", (e) => {
      console.error(e);
      reject(e);
    });
    if (stdinStream.push(Translate.encode(fen))) {
      stdinStream.push(null); // Signals the end of the stream (EOF)
      startTime = new Date(); // reset starttime when data is read
    }
  });
  return p;
}

function reportGameOver(chess, plLast) {
  console.log("GAME OVER");
  console.log("Checkmate             :" + chess.in_checkmate());
  console.log("Insufficient material :" + chess.insufficient_material());
  console.log("Stalemate             :" + chess.in_stalemate());
  console.log("SEED                  :" + seed);

  // good if pl player did not lose
  process.exit(
    plLast &&
      (chess.in_checkmate() ||
        chess.in_stalemate() ||
        chess.insufficient_material())
      ? 0
      : 1
  );
}

function timeFmt(time) {
  if (time > 2000) {
    return (time / 1000).toFixed(2) + "s";
  } else {
    return time.toFixed(2) + "ms";
  }
}

// Make random moves until the game ends
function runTest(x, totalTime, chess) {
  if (chess.game_over()) {
    reportGameOver(chess, false);
    return;
  }
  var fens = generateBoards(chess);
  if (x < 1000) {
    console.log("\n\n");
    console.log("🔀 RANDOM: ");
    console.log(Translate.encode(chess.fen()));
    console.log("\n\n");
    run(chess.fen(), fens)
      .then(function ({ fen, time }) {
        var plChess = new Chess(fen);
        let newTotalTime = time + totalTime;
        let avgTime = newTotalTime / (x + 1);
        console.log(`🤖👌 PL OK (${timeFmt(time)}, avg: ${timeFmt(avgTime)})`);
        console.log(Translate.encode(plChess.fen()));
        if (x > 5 && avgTime > 30 * 1000) {
          console.log("GAME OVER: too slow");
          process.exit(1);
        } else {
          if (plChess.game_over()) {
            reportGameOver(plChess, true);
          } else {
            runTest(x + 1, newTotalTime, makeMove(plChess));
          }
        }
      })
      .catch((e) => {
        if ("msg" in e && "data" in e) {
          const { msg, data, err } = e;
          console.log("🤖❌ PL FAIL");
          console.log((data || "").replace(/^\n*/, ""));
          console.log(msg);
          console.log("stderr");
          console.log(err);

          console.log("SEED                  :" + seed_init);
        } else {
          console.log("unexpected error");
          console.log(e);
        }
        process.exit(1);
      });
  } else {
    console.log("More than 1000 moves");
    console.log("SEED                  :" + seed_init);
    process.exit(1);
  }
}

setSeed();
if (random() > 0.5) {
  console.log("Random sets the board up for you to start as white");
  runTest(0, 0, new Chess());
} else {
  console.log("Random is white and makes the first move");
  runTest(0, 0, makeMove(new Chess()));
}

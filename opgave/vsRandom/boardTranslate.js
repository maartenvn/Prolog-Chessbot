/*
 * Extra functions to translate fens to ASCII art
 * */

symbols = [];
symbols["K"] = "♔";
symbols["Q"] = "♕";
symbols["R"] = "♖";
symbols["B"] = "♗";
symbols["N"] = "♘";
symbols["P"] = "♙";
symbols["k"] = "♚";
symbols["q"] = "♛";
symbols["r"] = "♜";
symbols["b"] = "♝";
symbols["n"] = "♞";
symbols["p"] = "♟";

revSymbols = [];
revSymbols["♔"] = "K";
revSymbols["♕"] = "Q";
revSymbols["♖"] = "R";
revSymbols["♗"] = "B";
revSymbols["♘"] = "N";
revSymbols["♙"] = "P";
revSymbols["♚"] = "k";
revSymbols["♛"] = "q";
revSymbols["♜"] = "r";
revSymbols["♝"] = "b";
revSymbols["♞"] = "n";
revSymbols["♟"] = "p";

LINE8 = 0;
LINE1 = 7;
function encode(fen) {
  const [board, player, rok, enpassant, halfs, fulls] = fen.split(" ");
  const boardLines = board
    .replace(/[1-9]/g, (x) => " ".repeat(Number.parseInt(x, 10)))
    .replace(/[kqrbnpKQRBNP]/g, (x) => symbols[x])
    .split("/");
  if (!boardLines.every((x) => x.length == 8) || boardLines.length != 8) {
    console.log(boardLines);
    throw "invalid fen";
  }
  let outBoard = boardLines.map((line, i) => `${8 - i} ${line}`);
  outBoard[LINE1] += [
    " [",
    rok.includes(revSymbols["♕"]) ? "♕" : " ",
    rok.includes(revSymbols["♔"]) ? "♔" : " ",
    enpassant.endsWith("3") ? enpassant : "",
    "]",
    player == "w" ? "☚" : "",
  ].join("");
  outBoard[LINE8] += [
    " [",
    rok.includes(revSymbols["♛"]) ? "♛" : " ",
    rok.includes(revSymbols["♚"]) ? "♚" : " ",
    enpassant.endsWith("6") ? enpassant : "",
    "]",
    player == "b" ? "☚" : "",
  ].join("");
  return [...outBoard, "  abcdefgh"].join("\n");
}

function decode(ascii) {
  const lines = ascii.replace(/^\s*/, "").split("\n");
  const boardLines = lines.map((s) => s.slice(2, 8 + 2)).slice(0, 8);

  if (boardLines.length != 8 || !boardLines.every((x) => x.length === 8)) {
    console.log(boardLines);
    throw "Could not parse board";
  }

  const board = boardLines
    .join("/")
    .replace(/ +/g, (x) => x.length)
    .replace(/[^0-9/]/g, (x) => revSymbols[x]);

  const statusB = lines[LINE8].slice(10);
  const statusW = lines[LINE1].slice(10);
  const rok = [
    statusW.includes("♔") ? revSymbols["♔"] : "",
    statusW.includes("♕") ? revSymbols["♕"] : "",
    statusB.includes("♚") ? revSymbols["♚"] : "",
    statusB.includes("♛") ? revSymbols["♛"] : "",
  ].join("");

  return [
    board,
    statusW.includes("☚") ? "w" : "b",
    rok || "-",
    (statusW + statusB).replace(/[^a-z0-9]/g, "") || "-",
  ].join(" ");
}

function keepImportant(fen) {
  const [board, player, rok, enpassant, halfs, fulls] = fen.split(" ");
  return [board, player, rok, enpassant].join(" ");
}

module.exports = {
  encode: encode,
  decode: decode,
  symbols: symbols,
  revSymbols: revSymbols,
  keepImportant: keepImportant,
};

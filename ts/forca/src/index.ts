"use strict";

import promptSync from "prompt-sync";
import * as fs from "fs";

const prompt = promptSync();

const TOP_GALLOW = "X==:==\nX  :\n";
const BOTTOM_GALLOW = "X\n===========\n";
const GALLOWS = [
  "X\nX\nX\n",
  "X  O\nX\nX\n",
  "X  O\nX  |\nX\n",
  "X  O\nX /|\nX\n",
  "X  O\nX /|\\\nX\n",
  "X  O\nX /|\\\nX / \n",
  "X  O\nX /|\\\nX / \\\n",
];

function getGallows(num_errors: number) {
  return TOP_GALLOW + GALLOWS[num_errors] + BOTTOM_GALLOW;
}

function checkWord(chars: string, word: string): string {
  let tmp_word = "";
  for (let c of word) {
    tmp_word += chars.includes(c) ? c : ".";
  }
  return tmp_word;
}

function selectSecretWord(wordList: string[]): string {
  return wordList[Math.floor(Math.random() * wordList.length)]
    .toLowerCase()
    .trim();
}

function playerName(): string {
  let namePlayer = prompt("Digite o nome do jogador: ");
  if (namePlayer === "") namePlayer = "Não fornecido";
  return namePlayer;
}

function getAChar(chars: string): string {
  let theChar = prompt("Digite uma letra: ");
  theChar = theChar.toLowerCase().trim();
  if (theChar == "") {
    console.log("Não pode ser vazio!");
    return getAChar(chars);
  } else if (chars.includes(theChar)) {
    console.log("Já digitou essa letra!");
    return getAChar(chars);
  } else return theChar;
}

function saveRecordsJson(filePath: string, records: Map<string, number>) {
  fs.writeFileSync(filePath, JSON.stringify(Array.from(records.entries())));
}

function loadRecordsJson(filePath: string): Map<string, number> {
  return new Map<string, number>(
    JSON.parse(fs.readFileSync(filePath, "utf-8"))
  );
}

function saveWordsJson(filePath: string, words: string[]) {
  fs.writeFileSync(filePath, JSON.stringify(words));
}

function loadWordsJson(filePath: string): string[] {
  return JSON.parse(fs.readFileSync(filePath, "utf-8"));
}

function listRecords(name: string, num_errors: number) {
  let records_tmp = new Map<string, number>();
  let records = loadRecordsJson("forcarecordes.json");
  if (records.has(name)) {
    if ((records.get(name) ?? 10) > num_errors) records.set(name, num_errors);
  } else {
    records.set(name, num_errors);
  }
  records_tmp = new Map([...records].sort((a, b) => a[1] - b[1]));
  records_tmp = new Map(Array.from(records_tmp).slice(0, 5));
  saveRecordsJson("forcarecordes.json", records_tmp);
  console.log(records_tmp);
}

function game(
  charsDigited: string,
  secretWord: string,
  numErrors: number,
  player: string
) {
  console.log(getGallows(numErrors));

  if (numErrors < 6) {
    const theChar: string = getAChar(charsDigited);
    const wordBefore: string = checkWord(charsDigited, secretWord);
    const chars: string = charsDigited + theChar;
    const wordAfter: string = checkWord(chars, secretWord);
    console.log(`'${wordAfter}'\n`);

    if (wordBefore != wordAfter) {
      if (wordAfter == secretWord) {
        console.log("Acertou!!!\n");
        listRecords(player, numErrors);
      } else {
        game(chars, secretWord, numErrors, player);
      }
    } else {
      game(chars, secretWord, numErrors + 1, player);
    }
  } else {
    console.log("Você perdeu!");
    listRecords("", numErrors);
  }
}

(function () {
  game(
    "",
    selectSecretWord(loadWordsJson("./palavrasforca.json")),
    0,
    playerName()
  );
})();

// function loadFile(filePath: string): string[] {
//   let words = fs.readFileSync(filePath, "utf-8");
//   return words.split("\n");
// }

// function loadRecordFile(filePath: string): Map<string, number> {
//   let lines = loadFile(filePath);
//   let records = new Map<string, number>();
//   for (let w of lines) {
//     let ww = w.split(";");
//     records.set(ww[0], parseInt(ww[1]));
//   }
//   return records;
// }

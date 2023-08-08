"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const prompt_sync_1 = __importDefault(require("prompt-sync"));
const fs = __importStar(require("fs"));
const prompt = (0, prompt_sync_1.default)();
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
function getGallows(num_errors) {
    return TOP_GALLOW + GALLOWS[num_errors] + BOTTOM_GALLOW;
}
function checkWord(chars, word) {
    let tmp_word = "";
    for (let c of word) {
        tmp_word += chars.includes(c) ? c : ".";
    }
    return tmp_word;
}
function selectSecretWord(wordList) {
    return wordList[Math.floor(Math.random() * wordList.length)]
        .toLowerCase()
        .trim();
}
function playerName() {
    let namePlayer = prompt("Digite o nome do jogador: ");
    if (namePlayer === "")
        namePlayer = "Não fornecido";
    return namePlayer;
}
function getAChar(chars) {
    let theChar = prompt("Digite uma letra: ");
    theChar = theChar.toLowerCase().trim();
    if (theChar == "") {
        console.log("Não pode ser vazio!");
        return getAChar(chars);
    }
    else if (chars.includes(theChar)) {
        console.log("Já digitou essa letra!");
        return getAChar(chars);
    }
    else
        return theChar;
}
function saveRecordsJson(filePath, records) {
    fs.writeFileSync(filePath, JSON.stringify(Array.from(records.entries())));
}
function loadRecordsJson(filePath) {
    return new Map(JSON.parse(fs.readFileSync(filePath, "utf-8")));
}
function saveWordsJson(filePath, words) {
    fs.writeFileSync(filePath, JSON.stringify(words));
}
function loadWordsJson(filePath) {
    return JSON.parse(fs.readFileSync(filePath, "utf-8"));
}
function listRecords(name, num_errors) {
    var _a;
    let records_tmp = new Map();
    let records = loadRecordsJson("forcarecordes.json");
    if (records.has(name)) {
        if (((_a = records.get(name)) !== null && _a !== void 0 ? _a : 10) > num_errors)
            records.set(name, num_errors);
    }
    else {
        records.set(name, num_errors);
    }
    records_tmp = new Map([...records].sort((a, b) => a[1] - b[1]));
    records_tmp = new Map(Array.from(records_tmp).slice(0, 5));
    saveRecordsJson("forcarecordes.json", records_tmp);
    console.log(records_tmp);
}
function game(charsDigited, secretWord, numErrors, player) {
    console.log(getGallows(numErrors));
    if (numErrors < 6) {
        const theChar = getAChar(charsDigited);
        const wordBefore = checkWord(charsDigited, secretWord);
        const chars = charsDigited + theChar;
        const wordAfter = checkWord(chars, secretWord);
        console.log(`'${wordAfter}'\n`);
        if (wordBefore != wordAfter) {
            if (wordAfter == secretWord) {
                console.log("Acertou!!!\n");
                listRecords(player, numErrors);
            }
            else {
                game(chars, secretWord, numErrors, player);
            }
        }
        else {
            game(chars, secretWord, numErrors + 1, player);
        }
    }
    else {
        console.log("Você perdeu!");
        listRecords("", numErrors);
    }
}
(function () {
    game("", selectSecretWord(loadWordsJson("./palavrasforca.json")), 0, playerName());
})();

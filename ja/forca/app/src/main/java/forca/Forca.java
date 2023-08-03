package forca;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.HashMap;
import java.util.Map;

public class Forca {
    private String player = "";
    private int numErrors = 0;
    private String chars = "";
    private String secretWord = "";

    /**
     * Solicita e retorna o nome do jogador.
     * 
     * @return o nome do jogador
     * @throws IOException
     */
    private String getPlayerName() throws IOException {
        System.out.print("Digite o nome do jogador: ");
        return (new BufferedReader(
                new InputStreamReader(System.in))).readLine();
    }

    /**
     * Carrega um arquivo de texto retornando uma lista com as linhas do arquivo
     * 
     * @param filePath o caminho do arquivo
     * @return lista com as linhas do arquivo
     * @throws IOException
     */
    private static List<String> loadFile(String filePath) throws IOException {
        return Files.readAllLines(Paths.get(filePath), StandardCharsets.UTF_8);
    }

    /**
     * Grava uma lista de linhas em um arquivo de texto.
     * 
     * @param filePath o caminho do arquivo
     * @param records  Map com Key= nome, Value= número de erros
     * @throws IOException
     */
    private static void saveRecords(String filePath, Map<String, String> records) throws IOException {
        String s = "";
        for (String k : records.keySet()) {
            s += k + ";" + records.get(k) + "\n";
        }
        Files.writeString(Paths.get(filePath), s);
    }

    /**
     * Recebe uma lista de palavras, sorteia e retorna uma delas para jogar.
     * 
     * @param words lista de palavras
     * @return uma das palavras da lista
     */
    private static String getSecretWord(List<String> words) {
        return words.get(ThreadLocalRandom.current().nextInt(0, words.size()));
    }

    /**
     * Retorna o patíbulo com as partes do enforcado conforme o número de erros.
     * 
     * @param numErrors o número de erros
     * @return o patíbulo com as partes do enforcado conforme o número de erros
     */
    private static String getGallow(int numErrors) {
        String topGallow = "X==:==\nX  :\n";
        String bottomGallow = "X\n===========\n";
        String[] gallows = {
                "X\nX\nX\n",
                "X  O\nX\nX\n",
                "X  O\nX  |\nX\n",
                "X  O\nX /|\nX\n",
                "X  O\nX /|\\\nX\n",
                "X  O\nX /|\\\nX / \n",
                "X  O\nX /|\\\nX / \\\n",
        };
        return topGallow + gallows[numErrors] + bottomGallow;
    }

    /**
     * Recebe uma lista de caracteres e a palavra secreta. Retorna a palavra com os
     * caracteres ainda não descobertos mascarados por ponto.
     * 
     * @param chars      lista de caracteres já digitados
     * @param secretWord a palavra secreta
     * @return palavra com os caracteres ainda não descobertos mascarados por ponto.
     */
    private static String checkWord(String chars, String secretWord) {
        if (chars.length() == 0)
            return ".".repeat(secretWord.length());
        if (secretWord.length() == 0)
            return "";
        String tmp = "";
        for (int i = 0; i < secretWord.length(); i++) {
            if (chars.contains(secretWord.charAt(i) + ""))
                tmp += secretWord.charAt(i) + "";
            else
                tmp += ".";
        }
        return tmp;
    }

    /**
     * Recursivamente recebe uma lista de caracteres já digitados, solicita e
     * retorna um caracter ainda não digitado
     * 
     * @param chars lista de caracteres já digitados
     * @return um caracter ainda não digitado
     * @throws IOException
     */
    private static String getAChar(String chars) throws IOException {
        System.out.print("Digite uma letra: ");
        String entry = (new BufferedReader(
                new InputStreamReader(System.in))).readLine();
        entry = entry.toLowerCase().trim().charAt(0) + "";
        if (chars.contains(entry)) {
            System.out.println("Você já digitou essa letra!!");
            return getAChar(chars);
        } else
            return entry;
    }

    /**
     * Imprime os recordistas do jogo.
     * 
     * @param records Map com Key= nome e Value= número de erros
     */
    private static void printRecords(Map<String, String> records) {
        Map<String, String> mRecordsSorted = sortRecords(records);
        System.out.println("\nRecordistas\n============");
        for (String k : mRecordsSorted.keySet()) {
            System.out.println(mRecordsSorted.get(k) + " " + k);
        }
    }

    /**
     * Ordena o Map com os recordista pelo número de erros. Limita a 5 registros.
     * 
     * @param records Map com Key= nome e Value = número de erros
     * @return Map com Key= número de erros e Value = nome
     */
    private static Map<String, String> sortRecords(Map<String, String> records) {
        Map<String, String> mRecordsSorted = new HashMap<String, String>();
        int i = 0;
        for (String v : records.keySet()) {
            i += 1;
            if (i <= 5)
                mRecordsSorted.put(records.get(v), v);
        }
        return mRecordsSorted;
    }

    /**
     * Carrega recordes do jogo de arquivo de texto. Atualiza com o novo recordista
     * se for o caso. Imprime os recordista.
     */
    private void getRecords() throws IOException {
        String arquivo = "forcarecordes.txt";
        List<String> records = loadFile(arquivo);
        Map<String, String> mRecords = new HashMap<String, String>();
        if (this.numErrors < 6) {
            for (String line : records) {
                String[] record = line.split(";");
                if (mRecords.containsKey(this.player)) {
                    if (Integer.parseInt(mRecords.get(record[1])) > this.numErrors)
                        mRecords.put(this.player, Integer.toString(this.numErrors));
                } else
                    mRecords.put(this.player, Integer.toString(this.numErrors));
            }
            saveRecords(arquivo, mRecords);
        }
        printRecords(mRecords);
    }

    /**
     * Rotina recursiva principal do jogo. Exibe o patíbulos. Solicita caracter.
     * Checa se acertou a palavra. Exibe palavra secreta mascarada. Se acertou a
     * palava exibe mensagem de vitória e imprime recordes. Se atingiu o número de
     * erros exibe mensagem de derrota e exibe recordes. Caso contrário continua.
     * 
     * @throws IOException
     */
    private void game() throws IOException {
        System.out.println(getGallow(this.numErrors));
        if (this.numErrors < 6) {
            String theChar = getAChar(this.chars);
            String wordBef = checkWord(this.chars, this.secretWord);
            this.chars += theChar;
            String wordAft = checkWord(this.chars, this.secretWord);
            System.out.println("'" + wordAft + "'");
            if (wordAft != wordBef) {
                if (wordAft == this.secretWord) {
                    System.out.println("Você acertou!!");
                    getRecords();
                } else {
                    game();
                }
            } else {
                this.numErrors += 1;
                game();
            }

        } else {
            System.out.println("Você perdeu!!");
            getRecords();
        }
    }

    /**
     * Rotina principal do jogo. Único metodo publico exposto da Classe.
     * 
     * @throws IOException
     */
    public void play() throws IOException {
        this.player = getPlayerName();
        this.secretWord = getSecretWord(loadFile("palavrasforca.txt"));
        game();
    }

}

"""_summary_
"""
import itertools
import random
import time

TOP_GALLOW = "X==:==\nX  :\n"
BOTTOM_GALLOW = "X\n===========\n"
GALLOWS = [
    "X\nX\nX\n",
    "X  O\nX\nX\n",
    "X  O\nX  |\nX\n",
    "X  O\nX /|\nX\n",
    "X  O\nX /|\\\nX\n",
    "X  O\nX /|\\\nX / \n",
    "X  O\nX /|\\\nX / \\\n",
]
RECORDS_FILE_PATH = "forcarecordes.txt"


def load_file(file_path):
    """Carrega um arquivo de texto retornando uma lista com as linhas do arquivo

    Args:
        filePath (_type_): caminho do arquivo relativo ao executável

    Returns:
        String List: Lista com as linhas do arquivo
    """
    word_list = []
    with open(file_path, "r", encoding="utf-8") as file:
        for line in file.readlines():
            word_list.append(line)
    return word_list


def save_records(records, file_path):
    """Grava uma lista de linhas em um arquivo de texto. apenas os cinco primeiros são mantidos.

    Args:
        records (String Dict): Dicionário com nome dos recordistas (key) e número de erros (value)
        file_path (String): caminho do arquivo relativo ao executável
    """
    with open(file_path, "w", encoding="utf-8") as record_file:
        for key, value in records.items():
            record_file.write(f"{key:s};{value:d}\n")


def list_records(name, num_errors):
    """Carrega, atualiza e exibe os recordistas do jogo.

    Args:
        name (String): nome do jogador ou vazio se quer apenas exibir os recordistas
        num_errors (int): número de erros
    """
    records_dict = {}
    records_lines = load_file(RECORDS_FILE_PATH)
    for line in records_lines:
        data = line.split(sep=";")
        records_dict[data[0]] = int(data[1])
    if name != "":
        if (name in records_dict) and (records_dict[name] > num_errors):
            records_dict[name] = num_errors
        else:
            records_dict[name] = num_errors
        records_dict = dict(sorted(records_dict.items(), key=lambda item: item[1]))
        records_dict = dict(itertools.islice(records_dict.items(), 5))
        save_records(records_dict, RECORDS_FILE_PATH)
    print("\nRecordistas")
    print("============")
    for key, value in records_dict.items():
        print(f"{key:20s} {value:d}")


def player_name():
    """Solicita e retorna o nome do jogador.

    Returns:
        String: nome do jogador
    """
    name = input("Digite o nome do jogador: ")
    return name


def select_secret_word(word_list):
    """Recebe uma lista de palavras, sorteia e retorna uma delas para jogar.

    Args:
        word_list (String list): Lista com palavras a serem sorteadas

    Returns:
        String: a palavra a ser descoberta no jogo
    """
    index = random.randint(0, len(word_list))
    word = word_list[index].lower().strip()
    return word


def get_gallows(num_errors):
    """Retorna o patíbulo com as partes do enforcado conforme o número de erros.

    Args:
        num_errors (int): o número de erros

    Returns:
        String: patíbulo com as partes do enforcado conforme o número de erros
    """
    return TOP_GALLOW + GALLOWS[num_errors] + BOTTOM_GALLOW


def check_word(chars, word):
    """Recebe uma lista de caracteres e a palavra secreta. Retorna a palavra com os caracteres
    ainda não descobertos mascarados por ponto.

    Args:
        chars (Char List): caracteres já digitados pelo jogador
        word (String): a palavra a ser descoberta no jogo

    Returns:
        String: a palavra com os caracteres ainda não descobertos mascarados por ponto
    """
    tmp_word = ""
    for char in word:
        tmp_word += char if char in chars else "."
    return tmp_word


def get_a_char(chars):
    """Recebe uma lista de caracteres já digitados, solicita e retorna um caracter
    ainda não digitado

    Args:
        chars (Char List): lista de caracteres já digitados

    Returns:
        char: um caracteres que ainda não digitado
    """
    the_char = input("\nDigite uma letra:").lower().strip()
    if the_char in chars:
        print("Você já tentou esta letra!")
        return get_a_char(chars)
    return the_char


def game(chars, secret_word, num_errors, player):
    """Fluxo principal do Jogo da Forca. Recursivo. Recebe a lista dos caracteres digitados, a
    palavra secreta, o número de erros e o nome do jogador.
    Solicita caracter. Confere se acertou a palavra.
    Se atingiu o numero maximo de erros e exibe mensagem, exibe recordes e encerra.
    Se caracter digitado pertence à palavra secreta mas não a completou
    atualiza a lista de caracteres e chama recursivamente a função sem aumentar o número de erros.
    Se caracter digitado pertence à palavra secreta e a completou
    exibe mensagem e imprime recordes.
    Se caracter digitado não pertence à palavra secreta atualiza a lista de caracteres e
    chama recursivamente a função aumentando em 1 o número de erros.

    Args:
        chars (Char Lista): caracteres já digitados
        secret_word (String): palavra a ser descoberta
        num_errors (int): quantidade de erros
        player (String): nome do jogador
    """
    print(get_gallows(num_errors))
    if num_errors < 6:
        the_char = get_a_char(chars)
        word_bef = check_word(chars, secret_word)
        chars += the_char
        word_aft = check_word(chars, secret_word)
        print(f"'{word_aft:s}'\n")
        if word_bef != word_aft:
            if word_aft == secret_word:
                print("Acertou!!!\n")
                list_records(player, num_errors)
            else:
                game(chars, secret_word, num_errors, player)
        else:
            game(chars, secret_word, (num_errors + 1), player)

    else:
        print("Você perdeu!\n")
        list_records("", 0)


def game_duration(start_of_game, end_of_game):
    """Calcula duração do jogo

    Args:
        start_of_game (float): time.time() do inicio do jogo
        end_of_game (float): time.time() do fim do jogo
    """
    duration_of_game = int(end_of_game - start_of_game)
    minutos = duration_of_game // 60
    segundos = duration_of_game % 60
    print(f"O jogo durou {minutos:d} minutos e {segundos:d} segundos.")


def main():
    """EntryPoint do programa.
    Carrega a lista de palavras e solicita o nome do jogador.
    Inicia o fluxo do jogo com uma lista vazia de caracteres digitados, a palavra secreta
    sorteada, número de erros zerado e o nome do jogador."""
    list_words = load_file("palavrasforca.txt")
    player = player_name()
    start_of_game = time.time()
    game([], (select_secret_word(list_words)), 0, player)
    game_duration(start_of_game, time.time())


main()

open System.Collections

/// Carrega um arquivo de texto retornando uma lista com as linhas do arquivo
let loadFile (filePath) =
    System.IO.File.ReadAllLines filePath |> Array.toList

/// Grava uma lista de linhas em um arquivo de texto
let saveFile (filePath: string, lines: string list) =
    System.IO.File.WriteAllLines(filePath, lines)

/// Carrega, atualiza e exibe os recordistas do jogo.
let getRecords (name: string, numErrors: int) =
    let mutable list_records =
        loadFile "forcarecordes.txt" |> List.map (fun x -> x.Split(";") |> Array.toList)

    if name <> "" then
        if list_records |> List.exists (fun x -> x[0] = name) then
            let index = list_records |> List.findIndex (fun x -> x[0] = name)

            if int (list_records[index][1]) > numErrors then
                list_records <- list_records |> List.removeAt index
                list_records <- List.append list_records [ [ name; numErrors.ToString() ] ]
        else
            list_records <- List.append list_records [ [ name; numErrors.ToString() ] ]

        list_records <- list_records |> List.sortBy (fun (x: string list) -> x[1])
        let list_records = list_records[0..4]
        saveFile ("forcarecordes.txt", list_records |> List.map (fun x -> x.Item(0) + ";" + x.Item(1)))

    printfn ("\nRecordistas")
    printfn ("============")

    for element in list_records do
        printfn "%20s %1s" (element.Item(0)) (element.Item(1).ToString())

    ()

/// Recebe uma lista de palavras, sorteia e retorna uma delas para jogar.
let selectSecretWord (words: string list) =
    let rnd = new System.Random()
    (words |> List.item (rnd.Next(words.Length))).ToLower().Trim()

/// Solicita e retorna o nome do jogador.
let playerName () =
    printf "Informe o nome do jogador: "
    System.Console.ReadLine()

/// Recebe uma lista de caracteres já digitados, solicita e retorna um caracter
/// ainda não digitado
let rec getAChar (chars: char list) =
    printf "Digite uma letra: "
    let theChar = System.Console.ReadLine().ToString().ToLower()[0]

    if chars |> List.contains (theChar) then
        printfn "Já tentou essa letra!"
        getAChar (chars)
    else
        theChar

/// Retorna o patíbulo com as partes do enforcado conforme o número de erros.
let getGallows (numErrors) =
    let gallows =
        [ "X\nX\nX\n"
          "X  O\nX\nX\n"
          "X  O\nX  |\nX\n"
          "X  O\nX /|\nX\n"
          "X  O\nX /|\\\nX\n"
          "X  O\nX /|\\\nX / \n"
          "X  O\nX /|\\\nX / \\\n" ]

    "X==:==\nX  :\n" + (gallows.Item numErrors) + "X\n===========\n"

/// Recebe uma lista de caracteres e a palavra secreta. Retorna a palavra com os caracteres
/// ainda não descobertos mascarados por ponto.
let checkWord (chars: char list, word: string) : string =
    word |> String.map (fun x -> if chars |> List.contains (x) then x else '.')

/// Fluxo principal do Jogo da Forca. Recursivo. Recebe a lista dos caracteres digitados, a
/// palavra secreta, o número de erros e o nome do jogador.
/// Solicita caracter. Confere se acertou a palavra.
/// Se atingiu o numero maximo de erros e exibe mensagem, exibe recordes e encerra.
/// Se caracter digitado pertence à palavra secreta mas não a completou
/// atualiza a lista de caracteres e chama recursivamente a função sem aumentar o número de erros.
/// Se caracter digitado pertence à palavra secreta e a completou
/// exibe mensagem e imprime recordes.
/// Se caracter digitado não pertence à palavra secreta atualiza a lista de caracteres e
/// chama recursivamente a função aumentando em 1 o número de erros.
let rec game (chars, secretWord, numErrors, player: string) =
    printfn "%s" (getGallows numErrors)

    if numErrors < 6 then
        let theChar = getAChar (chars)
        let wordBef = checkWord (chars, secretWord)
        let chars = List.append chars [ theChar ]
        let wordAft = checkWord (chars, secretWord)
        printfn "'%s'" wordAft

        if (wordBef <> wordAft) then
            if wordAft = secretWord then
                printfn "Acertou!!!"
                getRecords (player, numErrors)
            else
                game (chars, secretWord, numErrors, player)
        else
            game (chars, secretWord, (numErrors + 1), player)
    else
        printfn "Você perdeu!"
        getRecords ("", 0)

/// EntryPoint do programa.
/// Carrega a lista de palavras e solicita o nome do jogador.
/// Inicia o fluxo do jogo com uma lista vazia de caracteres digitados, a palavra secreta
/// sorteada, número de erros zerado e o nome do jogador.
[<EntryPoint>]
let main args =
    let list_words = loadFile "palavrasforca.txt"
    let player = playerName ()
    game ([], (selectSecretWord list_words), 0, player)
    0

namespace Adacola.ActivePattern

module FizzBuzzPattern =
    let (|Fizz|Buzz|FizzBuzz|Number|) x =
        match x % 3, x % 5 with
        | 0, 0 -> FizzBuzz
        | 0, _ -> Fizz
        | _, 0 -> Buzz
        | _ -> Number x

module FizzBuzz =
    open FizzBuzzPattern

    let fizzBuzz x =
        match x with
        | Fizz -> printfn "Fizz"
        | Buzz -> printfn "Buzz"
        | FizzBuzz -> printfn "FizzBuzz"
        | Number n -> printfn "%d" n

    (* 実行結果
    > [1 .. 20] |> List.iter fizzBuzz;;
    1
    2
    Fizz
    4
    Buzz
    Fizz
    7
    8
    Fizz
    Buzz
    11
    Fizz
    13
    14
    FizzBuzz
    16
    17
    Fizz
    19
    Buzz
    val it : unit = ()
    *)

module FizzBuzzChoice =
    open FizzBuzzPattern

    let fizzBuzzChoice x =
        let result = (|Fizz|Buzz|FizzBuzz|Number|) x
        match result with
        | Choice1Of4 () -> printfn "Fizz"
        | Choice2Of4 () -> printfn "Buzz"
        | Choice3Of4 () -> printfn "FizzBuzz"
        | Choice4Of4 n -> printfn "%d" n

    (*
    > [0 .. 20] |> List.iter fizzBuzzChoice;;
    FizzBuzz
    1
    2
    Fizz
    4
    Buzz
    Fizz
    7
    8
    Fizz
    Buzz
    11
    Fizz
    13
    14
    FizzBuzz
    16
    17
    Fizz
    19
    Buzz
    val it : unit = ()
    *)

    let (|MultiplesOf2|_|) x = match x % 2 with 0 -> Some(x / 2) | _ -> None
    let (|One|_|) = function 1 -> Some() | _ -> None
    // 以下のような書き方も可能。どちらも戻り値の型はunit optionとなる
    // let (|One|_|) = function 1 -> Some One | _ -> None
    let multiplesOf2OrOne n =
        match n with
        | MultiplesOf2 x -> printfn "%d = MultiplesOf2 : %d" n x
        | One -> printfn "%d = ONE" n          // unit optionの場合は結果を受け取る変数を省略可能
    //  | One x -> printfn "%d = ONE %A" n x   // あえてunitを受け取るような書き方も可能。この出力結果は「1 = ONE <null>」
        | _ -> printfn "%d = どちらでもない" n

    (* 実行結果
    > [1 .. 5] |> List.iter multiplesOf2OrOne;;
    1 = ONE
    2 = MultiplesOf2 : 1
    3 = どちらでもない
    4 = MultiplesOf2 : 2
    5 = どちらでもない
    val it : unit = ()
    *)

    // 最後以外の引数 x, y にはパターンマッチの中に書く引数が、 最後の引数の z にはmatch式の対象となる値が来る
    let (|Between|_|) x y z = if x < z && z < y then Some() else None
    let betweenTest n =
        match 2 with
        | Between 1 2 -> failwith "ここには来ない"
        | Between 2 3 -> failwith "ここには来ない"
        | Between 1 n -> printfn "2 is between 1 and %d" n
        | _ -> printfn "2 is not between 1 and %d" n

    (* 実行結果
    > [1 .. 4] |> List.iter betweenTest;;
    2 is not between 1 and 1
    2 is not between 1 and 2
    2 is between 1 and 3
    2 is between 1 and 4
    val it : unit = ()
    *)

    type SchoolIdolAttribute = Smile | Pure | Cool
    [<Measure>] type 年生
    let (|SchoolIdol|_|) grade attr =
        match grade, attr with
        | 2<年生>, Smile -> Some "穂乃果"
        | 2<年生>, Pure -> Some "ことり"
        | 2<年生>, Cool -> Some "海未"
        | 1<年生>, Smile -> Some "凛"
        | 1<年生>, Pure -> Some "花陽"
        | 1<年生>, Cool -> Some "真姫"
        | 3<年生>, Smile -> Some "にこ"
        | 3<年生>, Pure -> Some "希"
        | 3<年生>, Cool -> Some "絵里"
        | _ -> None

    let schoolIdolTest() =
        match Cool with
        | SchoolIdol 4<年生> _ -> failwith "ここには来ない"
        | SchoolIdol 2<年生> name -> printfn "%s" name
        | _ -> failwith "ここには来ない"

    (* 実行結果
    > schoolIdolTest();;
    海未
    val it : unit = ()
    *)

module LetActivePattern =
    let (|Hoge|) x = sprintf "hoge:%s" x
    let printHoge = function Hoge x -> printfn "%s" x

    (* 実行結果
    > printHoge "foo";;
    hoge:foo
    val it : unit = ()
    *)

/// 自然数
type NaturalNumber = private NaturalNumberImpl of bigint

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NaturalNumber =
    /// 「0は自然数」派の人のためのコンストラクタ
    let create n =
        if n < 0I then invalidArg "n" "自然数は0以上です！！！"
        NaturalNumberImpl n

    /// NaturalNumberの値を取り出すためのアクティブパターン
    let (|NaturalNumber|) (NaturalNumberImpl n) = n

module Example2 =
    open System.Net
    
    let (|WebProtocolError|_|) (e : exn) =
        match e with
        | :? WebException as e when e.Status = WebExceptionStatus.ProtocolError ->
            use response = e.Response :?> HttpWebResponse
            Some response.StatusCode
        | _ -> None

    let webProtocolErrorTest() =
        use client = new WebClient()
        // 例外のcatchもパターンマッチなのでアクティブパターンが適用可能
        try
            client.DownloadString("http://www.lovelive-anime.jp/notfound") |> ignore
        with
        | WebProtocolError HttpStatusCode.NotFound -> printfn "404 NotFound でした！"
        | WebProtocolError statusCode -> printfn "NotFoundではなくて %A でした" statusCode
        | :? WebException as e -> printfn "ProtocolError以外のWebExceptonでした : %O" e
        | e -> printfn "WebExcepton以外の例外でした : %O" e

    (*
    > webProtocolErrorTest();;
    404 NotFound でした！
    val it : unit = ()    
    *)

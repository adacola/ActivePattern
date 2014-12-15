namespace Adacola.ActivePattern

module OtherModule =
    // アクティブパターンを定義したモジュールをopenするとアクティブパターンが使用可能
    open NaturalNumber

    let zero = NaturalNumber.create 0I
    // エラー 型 'NaturalNumber' の共用体ケースまたはフィールドは、このコードの場所からアクセスできません
    // let n = NaturalNumber 1I

    // match式以外でもパターンマッチ可能
    let isEven (NaturalNumber n) = n % 2I = 0I

## chap1

- Cは自由な言語だ。*C makes it easy to shoot yourself in the foot.*

## chap2

- stack
- char型の変数は1byteのサイズ、1文字に1byteのメモリが必要。また、C言語は文字列型を持たない。よって配列に格納。配列の最後に*\0*を格納することで明示的に文字列であることを示す。
- *''* で文字を数値として扱ってくれる。
- *""* でくくられた文字列はconstな読み込み領域に確保される。
- *static*に変数を宣言することで、stackではないメモリ領域に保存できる。
- *const*指定子の再代入はコンパイラがエラーを吐いてくれる🤮

## chap3

- 条件式も値を持つ。true => value:1, false => value:0
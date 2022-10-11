# フクビキレンキン Scala版

## usage

FukubikiRenkin#solve メソッドを所持金を引数にして実行すると処理前後の金額とアイテム欄の動きを記録したオブジェクトが返る。
```scala
package com.github.haruyan.fukubiki

object Main {
  def main(args: Array[String]): Unit = {
    val result = FukubikiRenkin.solve(198)
    println(result)
  }
}
```

## 錬金の仕組み

[フクビキレンキン詳細](details.md)参照。
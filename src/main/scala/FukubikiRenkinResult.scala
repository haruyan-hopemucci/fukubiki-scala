package com.github.haruyan.fukubiki

/**
 * 連勤結果オブジェクト
 * @param prevMoney 処理前の所持金
 * @param resultMoney 処理後の所持金
 * @param progressSeq 売買記録
 */
case class FukubikiRenkinResult(
    prevMoney: Int,
    var resultMoney: Int,
    var progressSeq: Seq[FukubikiRenkinProgress]
)

/**
 * 売買記録データクラス
 * @param operation 売り/買い
 * @param targetItem 売る・買うアイテム
 * @param prevMoney 売買する前の金額
 * @param afterMoney 売買後の金額
 * @param afterInventory 売買後のアイテム欄
 */
case class FukubikiRenkinProgress(
    operation: FukubikiRenkinOperation,
    targetItem: Item,
    prevMoney: Int,
    afterMoney: Int,
    afterInventory: List[Item]
)

/**
 * 売買情報Enum
 */
sealed trait FukubikiRenkinOperation

object FukubikiRenkinOperation {
  /** 買いオペ */
  case object Buy extends FukubikiRenkinOperation
  /** 売りオペ */
  case object Sell extends FukubikiRenkinOperation
  /** ダミーオペ。初期状態の記録に用いる */
  case object NoOp extends FukubikiRenkinOperation
}

/**
 * アイテムデータクラス
 * @param name アイテム名
 * @param buyValue 買価
 * @param sellValue 売価
 */
case class Item(
    name: String,
    buyValue: Int,
    sellValue: Int
)
object Item {
  val HealHerb: Item = Item("やくそう", 8, 4)
  val AntidoteHerb: Item = Item("どくけしそう", 10, 8)
  val HolyWater: Item = Item("せいすい", 20, 10)
  val WyvernWing: Item = Item("キメラのつばさ", 25, 20)
  val LotteryTicket: Item = Item("ふくびきけん",0,53)
  val Dummy: Item = Item("ダミー", 0, 0)
}

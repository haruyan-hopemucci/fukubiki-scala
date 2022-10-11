package com.github.haruyan.fukubiki

object FukubikiRenkin {

  /** ふくびき連勤処理実行
    * @param money 所持金
    * @return 結果データオブジェクト
    */
  def solve(money: Int): FukubikiRenkinResult = {
//    println(canGetLotteryTicket(money))
    // アイテム欄の初期状態はやくそう1つのみ
    val inventory = List[Item](Item.HealHerb)
    val result = FukubikiRenkinResult(money, -1, Seq[FukubikiRenkinProgress]())
    result.progressSeq = result.progressSeq :+ FukubikiRenkinProgress(
      FukubikiRenkinOperation.NoOp,
      Item.Dummy,
      money,
      money,
      inventory
    )
    // 最初に購入するアイテムの決定
    val initByOpes: Seq[FukubikiRenkinProgress] = money match {
      case money if canGetLotteryTicket(money - 25) =>
//        println("キメラのつばさケース")
        Seq(buy(Item.WyvernWing, result.progressSeq.last))
      case money if canGetLotteryTicket(money - 20) =>
//        println("せいすいケース")
        Seq(buy(Item.HolyWater, result.progressSeq.last))
      case money if canGetLotteryTicket(money - 18) =>
        // どくけしそう＋やくそうパターン
        val ope1 = buy(Item.AntidoteHerb, result.progressSeq.last)
        val ope2 = buy(Item.HealHerb, ope1)
        Seq(ope1, ope2)
      case money if canGetLotteryTicket(money - 16) =>
        //        println("やくそうx2ケース")
        val ope1 = buy(Item.HealHerb, result.progressSeq.last)
        val ope2 = buy(Item.HealHerb, ope1)
        Seq(ope1, ope2)
      case money if canGetLotteryTicket(money - 10) =>
//        println("どくけしそうケース")
        Seq(buy(Item.AntidoteHerb, result.progressSeq.last))
      case money if canGetLotteryTicket(money - 8) =>
//        println("やくそうケース")
        Seq(buy(Item.HealHerb, result.progressSeq.last))
      case _ =>
//        println("見送りケース")
        Seq(buy(Item.HealHerb, result.progressSeq.last))
    }
    result.progressSeq = result.progressSeq ++ initByOpes

    // アイテム欄がやくそう1つになるまで売り買いを繰り返す
    result.progressSeq =
      result.progressSeq ++ renkinOperation(result.progressSeq.last)

    // 確定処理
    // 最終金額の設定
    result.resultMoney = result.progressSeq.last.afterMoney
    // progress先頭のダミー要素を除去
    result.progressSeq = result.progressSeq match {
      case _ :: left => left
    }
    result
  }

  /** アイテム売却→購入→ふくびきけんを得る過程を記録する関数
    * @param prevProgress 1つ前の状態
    * @return 追加する売買記録のSeq
    */
  def renkinOperation(
      prevProgress: FukubikiRenkinProgress
  ): Seq[FukubikiRenkinProgress] = {
    val inv = prevProgress.afterInventory
    val result: Seq[FukubikiRenkinProgress] = inv match {
      // アイテム欄がやくそう1つだけになったら空のSeqを返して再帰処理終了
      case inv
          if inv.length == 1 && inv.count(f => f == Item.HealHerb) == 1 =>
        return Seq()
      case inv if inv.count(f => f == Item.HealHerb) >= 2 =>
        // やくそう2つ以上ある場合は、やくそう2つ売る→薬草1つ買う、でふくびきけんゲット
        val p1 = sell(Item.HealHerb, prevProgress)
        val p2 = sell(Item.HealHerb, p1)
        val p3 = buy(Item.HealHerb, p2)
        Seq(p1, p2, p3)
      case inv if inv.count(f => f == Item.AntidoteHerb) > 0 =>
        // どくけしそう売り→やくそう買いでふくびきけんゲット
        val p1 = sell(Item.AntidoteHerb, prevProgress)
        val p2 = buy(Item.HealHerb, p1)
        Seq(p1, p2)
      case inv if inv.contains(Item.HolyWater) =>
        // せいすい売り→どくけしそう買いでふくびきけんゲット
        val p1 = sell(Item.HolyWater, prevProgress)
        val p2 = buy(Item.AntidoteHerb, p1)
        Seq(p1, p2)
      case inv if inv.contains(Item.WyvernWing) =>
        // キメラ売り→せいすい買いでふくびきけんゲット
        val p1 = sell(Item.WyvernWing, prevProgress)
        val p2 = buy(Item.HolyWater, p1)
        Seq(p1, p2)
      case inv if inv.contains(Item.LotteryTicket) =>
        // どのオペレーションにも該当せず、ふくびきけんが残った場合はふくびきけんを売却
        val p1 = sell(Item.LotteryTicket, prevProgress)
        Seq(p1)
    }
    // 再帰呼び出しでワンモアセッ
    result ++ renkinOperation(result.last)
  }

  /** 指定アイテムを購入するオペレーション
    * @param target 購入するアイテム
    * @param prevProgress 購入前のprogress
    * @return 購入後のprogress
    */
  def buy(
      target: Item,
      prevProgress: FukubikiRenkinProgress
  ): FukubikiRenkinProgress = {
    val prevMoney = prevProgress.afterMoney
    val inv = prevProgress.afterInventory
    val ticket =
      if (canGetLotteryTicket(prevMoney - target.buyValue))
        Option(Item.LotteryTicket)
      else None
    val addInv = List(Option(target), ticket).flatten
    FukubikiRenkinProgress(
      operation = FukubikiRenkinOperation.Buy,
      targetItem = target,
      prevMoney = prevMoney,
      afterMoney = prevMoney - target.buyValue,
      afterInventory = inv ++ addInv
    )
  }

  /** 指定アイテムを売却するオペレーション
   * 指定アイテムが複数ばる場合は、先頭のものを除去します。
    * @param target 売却対象アイテム
    * @param prevProgress 操作前progress
    * @return 操作後progress
    */
  def sell(
      target: Item,
      prevProgress: FukubikiRenkinProgress
  ): FukubikiRenkinProgress = {
    val prevMoney = prevProgress.afterMoney
    val inv = prevProgress.afterInventory
    var removeFlg = false
    val afterInv: List[Item] = inv.foldLeft(List[Item]()) { (acm, elem) =>
      if (elem == target && !removeFlg) {
        removeFlg = true
        acm
      } else {
        acm :+ elem
      }
    }
    FukubikiRenkinProgress(
      operation = FukubikiRenkinOperation.Sell,
      targetItem = target,
      prevMoney = prevMoney,
      afterMoney = prevMoney + target.sellValue,
      afterInventory = afterInv
    )

  }

  /** 引数の所持金でふくびきけんを得ることができるかどうかをチェックする
    * ふくびきけんは所持金のそれぞれの桁の数字を加算し、5の倍数であればもらうことができる
    * @param money 所持金
    * @return ふくびきけんを得ることができるのであればtrue
    */
  def canGetLotteryTicket(money: Int): Boolean = {
    var nums = List[Int]()
    var m = money
    while (m > 0) {
      nums = (m % 10) :: nums
      m = m / 10
    }
    val num = nums.sum
    num % 5 == 0
  }

}

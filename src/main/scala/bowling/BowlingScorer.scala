package bowling

sealed trait BowlingFrame {
  def b1: Int
  def b2: Option[Int]
  def score:Int = b1+b2.getOrElse(0)
}

case class Frame(b1:Int, b2:Option[Int]) extends BowlingFrame
case class SpareFrame(b1:Int) extends BowlingFrame {
  val b2=Some(10-b1)
}
case object StrikeFrame extends BowlingFrame {
  val b1=10
  val b2=None
}
case class BonusFrame(b1:Int, b2:Option[Int]) extends BowlingFrame

object BowlingScorer {
  def scoreGame(game: String): Int = {
    score(parse(game));
  }

  def parse(game:String): List[BowlingFrame] = {
    val lineParts = game.split("\\|\\|")
    if (lineParts.length==0) return List()
    val mainFrames = lineParts(0).split('|').map(parseFrame).toList
    if (lineParts.length ==2) mainFrames:+parseBonusFrame(lineParts(1)) else mainFrames
  }

  def parseFrame(frame:String) : BowlingFrame = {
    val isSimpleFrame = "(\\d|-)(\\d|-)".r
    val isSpareFrame = "(\\d)/".r
    frame match {
      case isSimpleFrame(b1,b2) => Frame(ballToInt(b1),Some(ballToInt(b2)))
      case isSpareFrame(b1) => SpareFrame(ballToInt(b1))
      case "X" => StrikeFrame
    }
  }

  def parseBonusFrame(frame:String) : BowlingFrame = {
    val isBonusFrame = "(\\d|-|X)(\\d|-|X)?".r
    frame match {
      case isBonusFrame(b1,b2) => BonusFrame(ballToInt(b1), if (b2==null) None else Some(ballToInt(b2)))
    }
  }

  def score(frame:List[BowlingFrame]) : Int = {
    frame match {
      case (f1@ StrikeFrame)::(f2@ StrikeFrame)::f3::xs => f1.score + f2.score + f3.b1 + score(f2::f3::xs)
      case (f1@ StrikeFrame)::f2::xs => f1.score + f2.score + score(f2::xs)
      case (f1@ SpareFrame(_))::f2::xs => f1.score + f2.b1 + score(f2::xs)
      case BonusFrame(_,_)::nil => 0
      case f1::xs => f1.score + score(xs)
      case nil => 0
    }
  }

  def ballToInt(ball: String) : Int = {
    val isDigit = "(\\d)".r
    ball match {
      case "-" => 0
      case "X" => 10
      case isDigit(d) => d.toInt
    }
  }
}
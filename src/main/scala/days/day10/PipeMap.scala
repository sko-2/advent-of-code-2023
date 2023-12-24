package days.day10

class PipeMap(
  val map: Array[Array[MapTile]]
):
  def getByIndex(index: (Int, Int)): MapTile = map(index._1)(index._2)
  def mapWidth: Int = map.head.length
  def mapHeight: Int = map.length

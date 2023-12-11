package days.day5

case class MappingDetail(
  sourceStartingIndex: Long,
  destinationStartingIndex: Long,
  numberOfMatchingIndices: Long
):
  def apply(input: Long): Long =
    if isApplicable(input) then
      destinationStartingIndex + (input - sourceStartingIndex)
    else
      input

  def applyInverse(input: Long): Long =
    if isInverseApplicable(input) then
      sourceStartingIndex + (input - destinationStartingIndex)
    else
      input

  def isApplicable(input: Long): Boolean =
    input >= sourceStartingIndex && input < (sourceStartingIndex + numberOfMatchingIndices)

  def isInverseApplicable(input: Long): Boolean =
    input >= destinationStartingIndex && input < (destinationStartingIndex + numberOfMatchingIndices)

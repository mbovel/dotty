package dotty.tools.benchmarks.scripts

extension (self: Seq[Double])
  /** Returns the `p`-th percentile of the sequence.
    *
    * @param self
    *   the sequence to compute the percentile of, sorted in ascending order.
    * @param p
    *   the percentile to compute, in the range [0.0, 1.0].
    */
  def percentile(p: Double): Double =
    require(p >= 0.0 && p <= 1.0, s"Percentile must be in the range [0.0, 1.0], but was $p.")
    require(self.nonEmpty, "Cannot compute the percentile of an empty sequence.")
    //require(self == self.sorted, "The sequence must be sorted in ascending order.")

    val index = p * (self.length - 1)
    if index.isValidInt then
      self(index.toInt)
    else
      val i = index.toInt
      val j = i + 1
      self(i) * (j - index) + self(j) * (index - i)

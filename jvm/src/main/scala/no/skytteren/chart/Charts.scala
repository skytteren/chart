package no.skytteren.chart

import scalatags.text

object Charts extends Chart:
  override type Builder = text.Builder
  override type FragT = String
  override type Output = String
  override type Bundle = scalatags.Text.type
  override val bundle = scalatags.Text

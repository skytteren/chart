package no.skytteren.chart

import org.scalajs.dom

object Charts extends Chart:
  override type Builder = dom.Element
  override type FragT = dom.Node
  override type Output = dom.Element
  override type Bundle = scalatags.JsDom.type
  override val bundle = scalatags.JsDom

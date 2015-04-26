package com.alexknvl.cse6331

import org.scalameter._
import org.scalameter.reporting._

import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.renderer.xy.DeviationRenderer
import org.jfree.chart.renderer.category.BarRenderer
import org.jfree.data.category.DefaultCategoryDataset

import scalax.chart.Chart
import scalax.chart.api._

import java.io._
import collection._
import utils.Tree
import utils.Statistics._
import java.awt.BasicStroke
import java.awt.Color
import Key.reports._
import java.text.DateFormat.{getDateTimeInstance, MEDIUM}
import java.util.Date
import org.jfree.chart.{LegendItemCollection, LegendItem}
import org.jfree.chart.labels.{ItemLabelAnchor, ItemLabelPosition, StandardCategoryItemLabelGenerator}
import org.jfree.ui.TextAnchor
import scala.math.Pi

case class XYLine(transformX: (Double => Double), transformY: (Double => Double)) extends ChartReporter.ChartFactory {
  import Key._

  private implicit val MyToCategoryDatasetConverter: ToCategoryDataset[Seq[(String, (String, Double))]] =
    ToCategoryDataset { coll =>
      coll.foldLeft(new DefaultCategoryDataset) { case (dataset, (series, (category, value))) =>
        dataset.addValue(value.toDouble, series, category)
        dataset
      }
    }

  def createChart(scopename: String, cs: Seq[CurveData], histories: Seq[History], colors: Seq[Color] = Seq()): Chart = {
    val dataset = for ((curve, idx) <- cs.zipWithIndex) yield {
      val seriesName = curve.context.goe(dsl.curve, idx.toString)

      val seriesData = for {
        measurement <- curve.measurements
        x = measurement.params.axisData.head._2.asInstanceOf[Int]
        y = measurement.value
      } yield transformX(x) -> transformY(y)

      seriesName -> seriesData
    }

    val chart = XYLineChart(dataset)
    chart.title = scopename
    chart.plot.domain.axis.label = cs.head.measurements.head.params.axisData.head._1
    chart.plot.range.axis.label = "value"

    chart.plot.setBackgroundPaint(new java.awt.Color(180, 180, 180))
    chart.antiAlias = true

    val renderer = new XYLineAndShapeRenderer()
    for (i <- cs.indices) renderer.setSeriesShapesVisible(i, true)
    chart.plot.setRenderer(renderer)

    chart
  }
}
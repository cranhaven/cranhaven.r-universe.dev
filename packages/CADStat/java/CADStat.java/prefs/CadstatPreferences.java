/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.neptuneinc.cadstat.prefs;

/**
 *
 * @author Pasha Minallah
 */
public class CadstatPreferences
{
  public static final Float DEFAULT_MAGNIFICATION = 1.0F;
  
  private PlotCharacter plotCharacter;
  private DataPointColor dataPointColor;
  private Float plotTitleMagnification;
  private Float axisMagnification;
  private Float axisLabelMagnification;
  private Float dataPointMagnification;
  
  public CadstatPreferences()
  {
    this.setDataPointColor(DataPointColor.BLACK);
    this.setPlotCharacter(PlotCharacter.OPEN_CIRCLE);
    this.setPlotTitleMagnification(DEFAULT_MAGNIFICATION);
    this.setAxisMagnification(DEFAULT_MAGNIFICATION);
    this.setAxisLabelMagnification(DEFAULT_MAGNIFICATION);
    this.setDataPointMagnification(DEFAULT_MAGNIFICATION);
  }
  
  public PlotCharacter getPlotCharacter()
  {
    return plotCharacter;
  }

  public void setPlotCharacter(PlotCharacter plotCharacter)
  {
    this.plotCharacter = plotCharacter;
  }

  public DataPointColor getDataPointColor()
  {
    return dataPointColor;
  }

  public void setDataPointColor(DataPointColor dataPointColor)
  {
    this.dataPointColor = dataPointColor;
  }

  public Float getPlotTitleMagnification()
  {
    return plotTitleMagnification;
  }

  public void setPlotTitleMagnification(Float plotTitleMagnification)
  {
    this.plotTitleMagnification = plotTitleMagnification;
  }

  public Float getAxisMagnification()
  {
    return axisMagnification;
  }

  public void setAxisMagnification(Float axisMagnification)
  {
    this.axisMagnification = axisMagnification;
  }

  public Float getAxisLabelMagnification()
  {
    return axisLabelMagnification;
  }

  public void setAxisLabelMagnification(Float axisLabelMagnification)
  {
    this.axisLabelMagnification = axisLabelMagnification;
  }

  public Float getDataPointMagnification()
  {
    return dataPointMagnification;
  }

  public void setDataPointMagnification(Float dataPointMagnification)
  {
    this.dataPointMagnification = dataPointMagnification;
  }
  
  @Override
  public String toString()
  {
    return "col=" + "'" + this.getDataPointColor().getValue() + "'"
      + ", pch=" + this.getPlotCharacter().getValue()
      + ", cex.main=" + this.getPlotTitleMagnification()
      + ", cex.axis=" + this.getAxisMagnification()
      + ", cex.lab=" + this.getAxisLabelMagnification()
      + ", cex=" + this.getDataPointMagnification();
  }
  
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.neptuneinc.cadstat.prefs;

/**
 *
 * @author Pasha Minallah
 */
public enum DataPointColor
{
  BLACK("Black", "black"),
  RED("Red", "darkred"),
  BLUE("Blue", "darkblue"),
  GREEN("Green", "darkgreen");
  
  private final String name;
  private final String value;
  
  DataPointColor(String name, String value)
  {
    this.name = name;
    this.value = value;
  }

  public String getName()
  {
    return name;
  }

  public String getValue()
  {
    return value;
  }
  
  @Override
  public String toString()
  {
    return this.getName();
  }
  
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.neptuneinc.cadstat.prefs;

/**
 *
 * @author Pasha Minallah
 */
public enum PlotCharacter
{
  OPEN_CIRCLE("Open Circle", 1),
  SQUARE("Square", 15),
  CIRCLE("Circle", 16),
  TRIANGLE("Triangle", 17);
  
  private final String name;
  private final Integer value;
  
  PlotCharacter(String name, Integer value)
  {
    this.name = name;
    this.value = value;
  }

  public String getName()
  {
    return name;
  }

  public Integer getValue()
  {
    return value;
  }
  
  @Override
  public String toString()
  {
    return this.getName();
  }
  
}

/*
 * RUtils.java
 *
 * Created on September 20, 2005, 9:25 PM
 */
package org.neptuneinc.cadstat.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Vector;

import org.rosuda.JGR.JGR;
import org.rosuda.REngine.REXP;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.REngine;
import org.rosuda.REngine.REngineException;

/**
 *
 * @author Pasha Minallah
 */
public class RUtils
{
  public static REngine R_ENGINE = JGR.getREngine();

  public static String[] ls() throws REngineException, REXPMismatchException
  {
    if (R_ENGINE == null)
    {
      return null;
    }

    String cmd = "ls()";
    REXP exp = R_ENGINE.parseAndEval(cmd);

    if (exp != null)
    {
      return exp.asStrings();
    }
    else
    {
      return null;
    }
  }

  public static Object[] ls(String rClass) throws REngineException, REXPMismatchException
  {
    if (R_ENGINE == null)
    {
      return null;
    }

    List vars = new ArrayList();
    String[] ls = ls();
    String var;

    for (int i = 0, n = ls.length; i < n; ++i)
    {
      var = ls[i];
      String cmd = "class(" + var + ")";
      REXP exp = R_ENGINE.parseAndEval(cmd);

      if (exp != null && !exp.isNull())
      {
        String varClass = null;

        if (exp.isString())
        {
          varClass = exp.asString();
        }

        // Add variable to list if classes match
        if (varClass != null && varClass.compareTo(rClass) == 0)
        {
          vars.add(var);
        }
      }
    }

    return (vars.toArray());
  }

  public static Object[] getDatasetList() throws REngineException, REXPMismatchException
  {
    return (RUtils.ls("data.frame"));
  }

  public static String[] colnames(String x) throws REngineException, REXPMismatchException
  {
    if (R_ENGINE == null)
    {
      return null;
    }

    String cmd = "colnames(" + x + ")";
    REXP exp = R_ENGINE.parseAndEval(cmd);

    if (exp != null)
    {
      return exp.asStrings();
    }
    else
    {
      return null;
    }
  }

  public static Vector colnamesVector(String x) throws REngineException, REXPMismatchException
  {
    String[] cols = colnames(x);
    Vector colvec = new Vector(cols.length);
    for (int i = 0, n = cols.length; i < n; ++i)
    {
      colvec.add(cols[i]);
    }
    return (colvec);
  }

  public static Vector factors(String x) throws REngineException, REXPMismatchException
  {
    String[] cols = colnames(x);

    if (cols != null)
    {
      Vector factors = new Vector(cols.length);

      String var, col;
      boolean isFactor;

      for (int i = 0, n = cols.length; i < n; ++i)
      {
        col = cols[i];
        var = x + "$" + col;

        String cmd = "as.character(is.factor(" + var + ") | is.logical(" + var + ") | is.character(" + var + "))";
        REXP exp = R_ENGINE.parseAndEval(cmd);

        if (exp != null)
        {
          isFactor = Boolean.valueOf(exp.asString()).booleanValue();

          if (isFactor)
          {
            factors.add(col);
          }
        }
      }

      return (factors);
    }
    else
    {
      return null;
    }
  }

  public static Vector nonFactors(String x) throws REngineException, REXPMismatchException
  {
    String[] cols = colnames(x);

    if (cols != null)
    {
      Vector nonFactors = new Vector(cols.length);

      String var, col;
      boolean isFactor;

      for (int i = 0, n = cols.length; i < n; ++i)
      {
        col = cols[i];
        var = x + "$" + col;
        String cmd = "as.character(is.factor(" + var + ") | is.logical(" + var + ") | is.character(" + var + "))";
        isFactor = Boolean.valueOf(R_ENGINE.parseAndEval(cmd).asString()).booleanValue();

        if (!isFactor)
        {
          nonFactors.add(col);
        }
      }

      return (nonFactors);
    }
    else
    {
      return null;
    }
  }

  public static Vector colnamesNumericVector(String x) throws REngineException, REXPMismatchException
  {
    String[] cols = colnames(x);

    if (cols != null)
    {
      Vector numerics = new Vector(cols.length);

      String var, col;
      boolean isNumeric;

      for (int i = 0, n = cols.length; i < n; ++i)
      {
        col = cols[i];
        var = x + "$" + col;

        String cmd = "is.numeric(" + var + ")";
        REXP exp = R_ENGINE.parseAndEval(cmd);

        if (exp != null)
        {
          isNumeric = Boolean.valueOf(exp.asString()).booleanValue();

          if (isNumeric)
          {
            numerics.add(col);
          }
        }
      }

      return (numerics);
    }
    else
    {
      return null;
    }
  }

  public static String toString(Object[] obj, String separator, String quote)
  {
    if (obj != null)
    {
      return RUtils.toString(Arrays.asList(obj), separator, quote);
    }
    else
    {
      return "";
    }
  }

  public static String toString(List list, String separator, String quote)
  {
    String str = "";

    if (separator == null)
    {
      separator = ",";
    }

    if (quote == null)
    {
      quote = "'";
    }

    if (list != null && list.size() > 0)
    {
      Object o;

      for (int i = 0, n = list.size(); i < n; ++i)
      {
        o = list.get(i);

        if (o != null)
        {
          if (o instanceof Integer || o instanceof Double)
          {
            str += o + separator;
          }
          else
          {
            str += quote + o + quote + separator;
          }
        }
      }

      if (str.length() >= separator.length())
      {
        str = str.substring(0, str.length() - separator.length());
      }
    }

    return str;
  }

  public static String getStringValue(String val)
  {
    if (val != null)
    {
      val = "'" + val + "'";
    }
    else
    {
      val = "NULL";
    }

    return val;
  }

  public static String getBooleanValue(Boolean val)
  {
    if (val != null && val)
    {
      return "T";
    }
    else
    {
      return "F";
    }
  }

  public static Vector evalAsVector(String cmd) throws REngineException, REXPMismatchException
  {
    if (R_ENGINE == null)
    {
      return null;
    }

    String cmd2;

    cmd2 = "as.vector(" + cmd + ")";
    REXP exp = R_ENGINE.parseAndEval(cmd2);

    if (exp != null && !exp.isNull())
    {
      Vector v;
      int[] intArray;
      double[] dblArray;
      String[] strArray;
      String str;

      if (exp.isInteger() && exp.isVector())
      {
        intArray = exp.asIntegers();
        v = new Vector(intArray.length);

        for (int i = 0, n = intArray.length; i < n; ++i)
        {
          v.add(new Integer(intArray[i]));
        }
      }
      else if (exp.isNumeric() && exp.isVector())
      {
        dblArray = exp.asDoubles();
        v = new Vector(dblArray.length);

        for (int i = 0, n = dblArray.length; i < n; ++i)
        {
          v.add(new Double(dblArray[i]));
        }
      }
      else if (exp.isString() && exp.isVector())
      {
        strArray = exp.asStrings();
        v = new Vector(strArray.length);

        for (int i = 0, n = strArray.length; i < n; ++i)
        {
          v.add(strArray[i]);
        }
      }
      else if (exp.isString())
      {
        str = exp.asString();
        v = new Vector(1);

        v.add(str);
      }
      else
      {
        cmd2 = "as.character(" + cmd + ")";
        strArray = R_ENGINE.parseAndEval(cmd2).asStrings();

        v = new Vector(strArray.length);

        for (int i = 0, n = strArray.length; i < n; ++i)
        {
          v.add(strArray[i]);
        }
      }

      return v;
    }
    else
    {
      return null;
    }
  }

  public static String evalAsString(String cmd) throws REngineException, REXPMismatchException
  {
    String str = null;

    if (R_ENGINE == null)
    {
      return str;
    }

    REXP exp = R_ENGINE.parseAndEval(cmd);

    if (exp != null && !exp.isNull() && exp.isString())
    {
      str = exp.asString();
    }

    return str;
  }
}

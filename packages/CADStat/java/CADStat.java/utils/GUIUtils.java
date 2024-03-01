/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.neptuneinc.cadstat.utils;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JRadioButton;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.REngineException;

/**
 *
 * @author Pasha Minallah
 */
public class GUIUtils
{
  public static void refreshDataset(JComboBox comboBox)
  {
    if (comboBox == null)
    {
      return;
    }

    try
    {
      GUIUtils.updateDataList(comboBox, RUtils.getDatasetList());
    }
    catch (REngineException ex)
    {
      Logger.getLogger(GUIUtils.class.getName()).log(Level.SEVERE, null, ex);
    }
    catch (REXPMismatchException ex)
    {
      Logger.getLogger(GUIUtils.class.getName()).log(Level.SEVERE, null, ex);
    }

    GUIUtils.refreshDatasetValidity(comboBox);
  }

  public static void refreshDatasetValidity(JComboBox comboBox)
  {
    if (comboBox != null)
    {
      comboBox.setEnabled(comboBox.getItemCount() > 0);
    }
  }

  public static void updateDataList(JComboBox comboBox, Object[] newItems)
  {
    if (comboBox == null)
    {
      comboBox = new JComboBox();
    }

    String selectedItem = GUIUtils.getSelectedItem(comboBox);

    if (newItems != null && newItems.length > 0)
    {
      comboBox.setModel(new DefaultComboBoxModel(newItems));

      if (selectedItem != null)
      {
        comboBox.setSelectedItem(selectedItem);
      }
      else
      {
        comboBox.setSelectedIndex(0);
      }
    }
    else
    {
      comboBox.removeAllItems();
    }
  }

  public static String getSelectedItem(JComboBox comboBox)
  {
    if (comboBox != null && comboBox.getItemCount() > 0)
    {
      Object item = comboBox.getSelectedItem();

      return (item != null ? item.toString() : null);
    }
    else
    {
      return null;
    }
  }

  public static String getSelectedItemR(JComboBox comboBox)
  {
    return RUtils.getStringValue(GUIUtils.getSelectedItem(comboBox));
  }

  public static Boolean getBooleanValue(JCheckBox checkBox)
  {
    if (checkBox != null)
    {
      return checkBox.isSelected();
    }
    else
    {
      return null;
    }
  }

  public static Boolean getBooleanValue(JRadioButton radioButton)
  {
    if (radioButton != null)
    {
      return radioButton.isSelected();
    }
    else
    {
      return null;
    }
  }

  public static String getBooleanValueR(JCheckBox checkBox)
  {
    return RUtils.getBooleanValue(GUIUtils.getBooleanValue(checkBox));
  }

  public static String getBooleanValueR(JRadioButton radioButton)
  {
    return RUtils.getBooleanValue(GUIUtils.getBooleanValue(radioButton));
  }

  public static Object[] getSelectedValues(JList list)
  {
    if (list != null && !list.isSelectionEmpty())
    {
      return list.getSelectedValues();
    }
    else
    {
      return null;
    }
  }
}

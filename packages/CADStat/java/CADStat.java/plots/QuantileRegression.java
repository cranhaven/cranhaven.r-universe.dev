/*
 * QuantileRegression.java
 *
 * Created on October 7, 2005, 9:26 AM
 */
package org.neptuneinc.cadstat.plots;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.event.TableModelEvent;
import org.neptuneinc.cadstat.ui.DataPlotDialog;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JTable;
import javax.swing.event.TableModelListener;
import javax.swing.table.TableModel;

import org.neptuneinc.cadstat.utils.GUIUtils;
import org.neptuneinc.cadstat.utils.RUtils;
import org.rosuda.JGR.JGR;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.REngineException;
import CADStat.java.plots.SubsetFormatter;

/**
 *
 * @author  Pasha Minallah
 */
public class QuantileRegression extends DataPlotDialog
{
  /**
   * Creates new form QuantileRegression
   */
  public QuantileRegression()
  {
    super();
  }

  /** Perform custom initialization. */
  @Override
  protected void initCustom()
  {
    this.initComponents();

    this.getPlotPane().add(plotPane, BorderLayout.CENTER);

    this.getDatasetPane().getDatasetComboBox().addActionListener(new ActionListener()
    {
      @Override
      public void actionPerformed(ActionEvent e)
      {
        datasetComboBoxActionPerformed(e);
      }
    });

    quantileTable.getModel().addTableModelListener(new TableModelListener()
    {
      @Override
      public void tableChanged(TableModelEvent e)
      {
        quantileTableTableChanged(e);
      }
    });

    this.refreshVariableList();
    this.refreshValidity();
    this.updateModelEquation();
  }

  private void datasetComboBoxActionPerformed(ActionEvent e)
  {
    this.refreshVariableList();
    this.refreshValidity();
    this.updateModelEquation();
  }

  private void quantileTableTableChanged(TableModelEvent e)
  {
    int row = e.getFirstRow();
    int col = e.getColumn();

    if (row == 0 && col == 0)
    {
      TableModel model = (TableModel) e.getSource();

      if (model.getValueAt(row, col) == null)
      {
        model.setValueAt(new Double(0.5), row, col);
      }
    }

    this.refreshValidity();
  }

  public void updateModelEquation()
  {
    if (this.getDatasetPane().getDatasetComboBox().getItemCount() > 0)
    {
      String modelEq = dependentComboBox.getSelectedItem() + " ~ "
        + RUtils.toString(independentList.getSelectedValues(), " + ", "");

      modelEqTextArea.setText(modelEq);
    }
    else
    {
      modelEqTextArea.setText(null);
    }
  }

  public void refreshVariableListValidity()
  {
    dependentLabel.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && dependentComboBox.getItemCount() > 0);
    dependentComboBox.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && dependentComboBox.getItemCount() > 0);
    independentLabel.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && independentList.getModel().getSize() > 0);
    independentList.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && independentList.getModel().getSize() > 0);
  }

  public void refreshVariableList()
  {
    if (this.getDatasetPane().getDatasetComboBox().getItemCount() > 0)
    {
      Vector nonFactors = null;

      try
      {
        nonFactors = RUtils.nonFactors(this.getDatasetPane().getSelectedDataset());

        dependentComboBox.setModel(new DefaultComboBoxModel(nonFactors));

        if (nonFactors != null)
        {
          independentList.setListData(nonFactors);
        }
        else
        {
          independentList.removeAll();
        }

        if (dependentComboBox.getItemCount() > 0)
        {
          dependentComboBox.setSelectedIndex(0);
        }
      }
      catch (REngineException ex)
      {
        Logger.getLogger(QuantileRegression.class.getName()).log(Level.SEVERE, null, ex);
      }
      catch (REXPMismatchException ex)
      {
        Logger.getLogger(QuantileRegression.class.getName()).log(Level.SEVERE, null, ex);
      }
    }
    else
    {
      dependentComboBox.removeAllItems();
      independentList.setModel(new DefaultListModel());
    }

    this.refreshValidity();
  }

  public void refreshConfLevelValidity()
  {
    confLabel.setEnabled(confIntervals.isSelected());
    confLevel.setEnabled(confIntervals.isSelected());
  }

  public void refreshScatterPlotValidity()
  {
    regScatterPlot.setEnabled(!independentList.isSelectionEmpty() && independentList.getSelectedIndices().length == 1);
  }

  public void refreshConfBandValidity()
  {
    TableModel model = quantileTable.getModel();
    int numValues = 0;

    for (int i = 0, n = model.getRowCount(); i < n; ++i)
    {
      if (model.getValueAt(i, 0) != null)
      {
        ++numValues;
      }
    }

    boolean enableConfBands = numValues >= 1 && !independentList.isSelectionEmpty() && independentList.getSelectedIndices().length == 1;
    plotCoeffConfBands.setEnabled(enableConfBands);
  }

  public void refreshSubmitButtonValidity()
  {
    this.getSubmitButton().setEnabled(!independentList.isSelectionEmpty());
  }

  public void refreshValidity()
  {
    this.refreshVariableListValidity();
    this.refreshConfLevelValidity();
    this.refreshConfBandValidity();
    this.refreshScatterPlotValidity();
    this.refreshSubmitButtonValidity();
  }

  /** This method is called from within the constructor to
   * initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is
   * always regenerated by the Form Editor.
   */
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {

    plotPane = new javax.swing.JPanel();
    modelEqPane = new javax.swing.JPanel();
    modelEqScrollPane = new javax.swing.JScrollPane();
    modelEqTextArea = new javax.swing.JTextArea();
    leftPane = new javax.swing.JPanel();
    varPane = new javax.swing.JPanel();
    dependentLabel = new javax.swing.JLabel();
    dependentComboBox = new javax.swing.JComboBox();
    independentLabel = new javax.swing.JLabel();
    independentScrollPane = new javax.swing.JScrollPane();
    independentList = new javax.swing.JList();
    analysisOptionsPane = new javax.swing.JPanel();
    confIntervals = new javax.swing.JCheckBox();
    confLabel = new javax.swing.JLabel();
    confLevel = new javax.swing.JSpinner();
    rightPane = new javax.swing.JPanel();
    quantilePane = new javax.swing.JPanel();
    quantileScrollPane = new javax.swing.JScrollPane();
    quantileTable = new javax.swing.JTable();
    plotOptionsPane = new javax.swing.JPanel();
    regScatterPlot = new javax.swing.JCheckBox();
    plotCoeffConfBands = new javax.swing.JCheckBox();

    modelEqPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Model Equation"));

    modelEqTextArea.setColumns(20);
    modelEqTextArea.setEditable(false);
    modelEqTextArea.setLineWrap(true);
    modelEqTextArea.setRows(2);
    modelEqTextArea.setWrapStyleWord(true);
    modelEqScrollPane.setViewportView(modelEqTextArea);

    javax.swing.GroupLayout modelEqPaneLayout = new javax.swing.GroupLayout(modelEqPane);
    modelEqPane.setLayout(modelEqPaneLayout);
    modelEqPaneLayout.setHorizontalGroup(
      modelEqPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, modelEqPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(modelEqScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 638, Short.MAX_VALUE)
        .addContainerGap())
    );
    modelEqPaneLayout.setVerticalGroup(
      modelEqPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(modelEqPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(modelEqScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(14, Short.MAX_VALUE))
    );

    varPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Variables"));

    dependentLabel.setText("Dependent:");

    dependentComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        dependentComboBoxActionPerformed(evt);
      }
    });

    independentLabel.setText("Independent:");

    independentList.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
      public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
        independentListValueChanged(evt);
      }
    });
    independentScrollPane.setViewportView(independentList);

    javax.swing.GroupLayout varPaneLayout = new javax.swing.GroupLayout(varPane);
    varPane.setLayout(varPaneLayout);
    varPaneLayout.setHorizontalGroup(
      varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(varPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
          .addComponent(independentLabel)
          .addComponent(dependentLabel))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(independentScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 184, Short.MAX_VALUE)
          .addComponent(dependentComboBox, javax.swing.GroupLayout.Alignment.TRAILING, 0, 184, Short.MAX_VALUE))
        .addContainerGap())
    );
    varPaneLayout.setVerticalGroup(
      varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(varPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(dependentComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(dependentLabel))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(independentLabel)
          .addComponent(independentScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 190, Short.MAX_VALUE))
        .addContainerGap())
    );

    analysisOptionsPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Analysis Options"));

    confIntervals.setText("Compute Confidence Intervals");
    confIntervals.addChangeListener(new javax.swing.event.ChangeListener() {
      public void stateChanged(javax.swing.event.ChangeEvent evt) {
        confIntervalsStateChanged(evt);
      }
    });

    confLabel.setText("Confidence Level:");

    confLevel.setModel(new javax.swing.SpinnerNumberModel(0.95d, 0.01d, 0.99d, 0.01d));

    javax.swing.GroupLayout analysisOptionsPaneLayout = new javax.swing.GroupLayout(analysisOptionsPane);
    analysisOptionsPane.setLayout(analysisOptionsPaneLayout);
    analysisOptionsPaneLayout.setHorizontalGroup(
      analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
        .addGroup(analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
            .addContainerGap()
            .addComponent(confIntervals))
          .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
            .addGap(61, 61, 61)
            .addComponent(confLabel)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(confLevel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    analysisOptionsPaneLayout.setVerticalGroup(
      analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(confIntervals)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(confLabel)
          .addComponent(confLevel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    javax.swing.GroupLayout leftPaneLayout = new javax.swing.GroupLayout(leftPane);
    leftPane.setLayout(leftPaneLayout);
    leftPaneLayout.setHorizontalGroup(
      leftPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addComponent(analysisOptionsPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
      .addComponent(varPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
    );
    leftPaneLayout.setVerticalGroup(
      leftPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, leftPaneLayout.createSequentialGroup()
        .addComponent(varPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(analysisOptionsPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
    );

    quantilePane.setBorder(javax.swing.BorderFactory.createTitledBorder("Quantiles"));

    quantileScrollPane.setPreferredSize(new java.awt.Dimension(200, 120));

    quantileTable.setModel(new javax.swing.table.DefaultTableModel(
      new Object [][] {
        {new Double(0.5)},
        {null},
        {null},
        {null},
        {null},
        {null},
        {null},
        {null}
      },
      new String [] {
        "Quantiles"
      }
    ) {
      Class[] types = new Class [] {
        java.lang.Double.class
      };

      public Class getColumnClass(int columnIndex) {
        return types [columnIndex];
      }
    });
    quantileTable.setCellSelectionEnabled(true);
    quantileScrollPane.setViewportView(quantileTable);

    javax.swing.GroupLayout quantilePaneLayout = new javax.swing.GroupLayout(quantilePane);
    quantilePane.setLayout(quantilePaneLayout);
    quantilePaneLayout.setHorizontalGroup(
      quantilePaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(quantilePaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(quantileScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 282, Short.MAX_VALUE)
        .addContainerGap())
    );
    quantilePaneLayout.setVerticalGroup(
      quantilePaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(quantilePaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(quantileScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 234, Short.MAX_VALUE)
        .addContainerGap())
    );

    plotOptionsPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Plot Options"));

    regScatterPlot.setText("Regression Scatterplot");

    plotCoeffConfBands.setText("Coefficient Confidence Bands");

    javax.swing.GroupLayout plotOptionsPaneLayout = new javax.swing.GroupLayout(plotOptionsPane);
    plotOptionsPane.setLayout(plotOptionsPaneLayout);
    plotOptionsPaneLayout.setHorizontalGroup(
      plotOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(plotOptionsPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(plotOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(regScatterPlot)
          .addComponent(plotCoeffConfBands))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    plotOptionsPaneLayout.setVerticalGroup(
      plotOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(plotOptionsPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(regScatterPlot)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(plotCoeffConfBands)
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    javax.swing.GroupLayout rightPaneLayout = new javax.swing.GroupLayout(rightPane);
    rightPane.setLayout(rightPaneLayout);
    rightPaneLayout.setHorizontalGroup(
      rightPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addComponent(plotOptionsPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
      .addComponent(quantilePane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
    );
    rightPaneLayout.setVerticalGroup(
      rightPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, rightPaneLayout.createSequentialGroup()
        .addComponent(quantilePane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(plotOptionsPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
    );

    javax.swing.GroupLayout plotPaneLayout = new javax.swing.GroupLayout(plotPane);
    plotPane.setLayout(plotPaneLayout);
    plotPaneLayout.setHorizontalGroup(
      plotPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(plotPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(plotPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
          .addComponent(modelEqPane, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
          .addGroup(javax.swing.GroupLayout.Alignment.LEADING, plotPaneLayout.createSequentialGroup()
            .addComponent(leftPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(rightPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    plotPaneLayout.setVerticalGroup(
      plotPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(plotPaneLayout.createSequentialGroup()
        .addComponent(modelEqPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(plotPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
          .addComponent(leftPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
          .addComponent(rightPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    setTitle("Quantile Regression");

    java.awt.Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
    setBounds((screenSize.width-716)/2, (screenSize.height-730)/2, 716, 730);
  }// </editor-fold>//GEN-END:initComponents

  private void confIntervalsStateChanged(javax.swing.event.ChangeEvent evt)//GEN-FIRST:event_confIntervalsStateChanged
  {//GEN-HEADEREND:event_confIntervalsStateChanged
    this.refreshValidity();
  }//GEN-LAST:event_confIntervalsStateChanged

private void dependentComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dependentComboBoxActionPerformed
  this.updateModelEquation();
}//GEN-LAST:event_dependentComboBoxActionPerformed

private void independentListValueChanged(javax.swing.event.ListSelectionEvent evt) {//GEN-FIRST:event_independentListValueChanged
  this.updateModelEquation();
  this.refreshValidity();
}//GEN-LAST:event_independentListValueChanged

  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JPanel analysisOptionsPane;
  private javax.swing.JCheckBox confIntervals;
  private javax.swing.JLabel confLabel;
  private javax.swing.JSpinner confLevel;
  private javax.swing.JComboBox dependentComboBox;
  private javax.swing.JLabel dependentLabel;
  private javax.swing.JLabel independentLabel;
  private javax.swing.JList independentList;
  private javax.swing.JScrollPane independentScrollPane;
  private javax.swing.JPanel leftPane;
  private javax.swing.JPanel modelEqPane;
  private javax.swing.JScrollPane modelEqScrollPane;
  private javax.swing.JTextArea modelEqTextArea;
  private javax.swing.JCheckBox plotCoeffConfBands;
  private javax.swing.JPanel plotOptionsPane;
  private javax.swing.JPanel plotPane;
  private javax.swing.JPanel quantilePane;
  private javax.swing.JScrollPane quantileScrollPane;
  private javax.swing.JTable quantileTable;
  private javax.swing.JCheckBox regScatterPlot;
  private javax.swing.JPanel rightPane;
  private javax.swing.JPanel varPane;
  // End of variables declaration//GEN-END:variables

  private List getColumn(JTable table, int column)
  {
    if (table != null && column <= table.getColumnCount())
    {
      List list = new ArrayList();

      for (int i = 0, n = table.getRowCount(); i < n; ++i)
      {
        list.add(table.getValueAt(i, column));
      }

      return (list);
    }
    else
    {
      return (null);
    }
  }

  @Override
  protected void submitButtonAction()
  {
    String cmd = "rq.JGR("
      + "my.data=" + this.getDatasetPane().getSelectedDataset()
      + ", subset1.name=" + (this.getFactorSelectionPane1().getFactorValueList().isSelectionEmpty() ? "NULL" : "'" + this.getFactorSelectionPane1().getSelectedFactor() + "'")
      + ", subset1.val=" + (this.getFactorSelectionPane1().getFactorValueList().isSelectionEmpty() ? "NULL" : SubsetFormatter.formatSubset(this.getFactorSelectionPane1().getSelectedFactorValues()))
      + ", subset2.name=" + (this.getFactorSelectionPane2().getFactorValueList().isSelectionEmpty() ? "NULL" : "'" + this.getFactorSelectionPane2().getSelectedFactor() + "'")
      + ", subset2.val=" + (this.getFactorSelectionPane2().getFactorValueList().isSelectionEmpty() ? "NULL" : SubsetFormatter.formatSubset(this.getFactorSelectionPane2().getSelectedFactorValues()))
      + ", my.formula=" + "'" + modelEqTextArea.getText() + "'"
      + ", my.tau=" + "c(" + RUtils.toString(getColumn(quantileTable, 0), ",", "'") + ")"
      + ", iCI=" + String.valueOf(confIntervals.isSelected()).toUpperCase()
      + (confIntervals.isSelected() ? ", conf.level=" + ((Double) confLevel.getValue()).doubleValue() : "")
      + (regScatterPlot.isEnabled() ? ", iScatterplot=" + String.valueOf(regScatterPlot.isSelected()).toUpperCase() : "")
      + ", iCIplot=" + String.valueOf(plotCoeffConfBands.isSelected()).toUpperCase()
      + ", dep.var=" + "'" + GUIUtils.getSelectedItem(dependentComboBox) + "'"
      + ", indep.var=" + "c(" + RUtils.toString(independentList.getSelectedValues(), ",", "'") + ")"
      + ")";
    JGR.MAINRCONSOLE.execute(cmd, true);
  }

  @Override
  protected void helpButtonAction()
  {
    String cmd = "CADStat.help('rq.JGR')";
    JGR.MAINRCONSOLE.execute(cmd, true);
    //JGR.MAINRCONSOLE.help("QuantileRegression");
  }

  @Override
  public void windowActivated(WindowEvent e)
  {
    Object dep = dependentComboBox.getSelectedItem();
    int[] indepIndices = independentList.getSelectedIndices();

    this.getDatasetPane().refreshDatasetComboBox();
    this.refreshFactorSelectionPanes();

    dependentComboBox.setSelectedItem(dep);
    independentList.setSelectedIndices(indepIndices);

    this.updateModelEquation();

    this.refreshValidity();
  }
}

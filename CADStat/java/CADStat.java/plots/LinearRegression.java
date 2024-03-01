/*
 * LinearRegression.java
 *
 * Created on September 20, 2005, 8:05 PM
 */
package org.neptuneinc.cadstat.plots;

import java.util.logging.Level;
import java.util.logging.Logger;
import org.neptuneinc.cadstat.ui.DataPlotDialog;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.util.Enumeration;
import java.util.Vector;
import javax.swing.AbstractButton;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
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
public class LinearRegression extends DataPlotDialog
{
  private String modelString;

  /**
   * Creates new form LinearRegression
   */
  public LinearRegression()
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

  public void refreshDistValidity()
  {
    Enumeration<AbstractButton> distButtonEnum = distButtonGroup.getElements();
    while (distButtonEnum.hasMoreElements())
    {
      distButtonEnum.nextElement().setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0);
    }
  }

  public void refreshModelValidity()
  {
    this.refreshDistValidity();
    this.refreshVariableListValidity();
  }

  public void updateModelEquation()
  {
    if (this.getDatasetPane().getDatasetComboBox().getItemCount() > 0)
    {
      String modelEq = dependentComboBox.getSelectedItem() + " ~ "
        + RUtils.toString(independentList.getSelectedValues(), " + ", "");

      modelEqTextArea.setText(modelEq);

      if (binomDistButton.isSelected())
      {
        this.setModelString(sampleSizeComboBox.getSelectedItem() + ","
          + dependentComboBox.getSelectedItem() + "," + RUtils.toString(independentList.getSelectedValues(), ",", ""));
      }
      else
      {
        this.setModelString(modelEq);
      }
    }
    else
    {
      this.setModelString(null);
      modelEqTextArea.setText(null);
    }
  }

  public void refreshVariableListValidity()
  {
    dependentLabel.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && dependentComboBox.getItemCount() > 0);
    dependentComboBox.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && dependentComboBox.getItemCount() > 0);
    independentLabel.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && independentList.getModel().getSize() > 0);
    independentList.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && independentList.getModel().getSize() > 0);
    sampleSizeLabel.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && sampleSizeComboBox.getItemCount() > 0 && binomDistButton.isSelected());
    sampleSizeComboBox.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && sampleSizeComboBox.getItemCount() > 0 && binomDistButton.isSelected());
  }

  public void refreshVariableList()
  {
    if (this.getDatasetPane().getDatasetComboBox().getItemCount() > 0)
    {
      Vector nonFactors = null, onePlusNonFactors = null, colNames = null;

      try
      {
        nonFactors = RUtils.nonFactors(this.getDatasetPane().getSelectedDataset());

        onePlusNonFactors = new Vector(nonFactors.size() + 1);
        onePlusNonFactors.add("1");
        onePlusNonFactors.addAll(nonFactors);

        colNames = RUtils.colnamesVector(this.getDatasetPane().getSelectedDataset());

        dependentComboBox.setModel(new DefaultComboBoxModel(nonFactors));
        sampleSizeComboBox.setModel(new DefaultComboBoxModel(onePlusNonFactors));

        if (colNames != null)
        {
          independentList.setListData(colNames);
        }
        else
        {
          independentList.removeAll();
        }

        if (dependentComboBox.getItemCount() > 0)
        {
          dependentComboBox.setSelectedIndex(0);
        }

        if (sampleSizeComboBox.getItemCount() > 0)
        {
          sampleSizeComboBox.setSelectedIndex(0);
        }
      }
      catch (REngineException ex)
      {
        Logger.getLogger(LinearRegression.class.getName()).log(Level.SEVERE, null, ex);
      }
      catch (REXPMismatchException ex)
      {
        Logger.getLogger(LinearRegression.class.getName()).log(Level.SEVERE, null, ex);
      }
    }
    else
    {
      dependentComboBox.removeAllItems();
      independentList.setModel(new DefaultListModel());
      sampleSizeComboBox.removeAllItems();
    }

    this.refreshValidity();
  }

  public void refreshConfLevelValidity()
  {
    confLabel.setEnabled(confIntervals.isSelected());
    confLevelSpinner.setEnabled(confIntervals.isSelected());
  }

  public void refreshSubmitButtonValidity()
  {
    this.getSubmitButton().setEnabled(!independentList.isSelectionEmpty());
  }

  public void refreshValidity()
  {
    this.refreshModelValidity();
    this.refreshConfLevelValidity();
    this.refreshSubmitButtonValidity();
  }

  /** This method is called from within the constructor to
   * initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is
   * always regenerated by the Form Editor.
   */
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {

    distButtonGroup = new javax.swing.ButtonGroup();
    plotPane = new javax.swing.JPanel();
    modelEqPane = new javax.swing.JPanel();
    modelEqScrollPane = new javax.swing.JScrollPane();
    modelEqTextArea = new javax.swing.JTextArea();
    plotBottomPane = new javax.swing.JPanel();
    leftPane = new javax.swing.JPanel();
    distPane = new javax.swing.JPanel();
    normDistButton = new javax.swing.JRadioButton();
    poisDistButton = new javax.swing.JRadioButton();
    binomDistButton = new javax.swing.JRadioButton();
    varPane = new javax.swing.JPanel();
    dependentLabel = new javax.swing.JLabel();
    dependentComboBox = new javax.swing.JComboBox();
    independentLabel = new javax.swing.JLabel();
    independentScrollPane = new javax.swing.JScrollPane();
    independentList = new javax.swing.JList();
    sampleSizeLabel = new javax.swing.JLabel();
    sampleSizeComboBox = new javax.swing.JComboBox();
    rightPane = new javax.swing.JPanel();
    plotOptsPane = new javax.swing.JPanel();
    resVsFit = new javax.swing.JCheckBox();
    normalQQ = new javax.swing.JCheckBox();
    scaleLoc = new javax.swing.JCheckBox();
    cookDist = new javax.swing.JCheckBox();
    influencePlot = new javax.swing.JCheckBox();
    analysisOptsPane = new javax.swing.JPanel();
    rmIntercept = new javax.swing.JCheckBox();
    confIntervals = new javax.swing.JCheckBox();
    confLabel = new javax.swing.JLabel();
    confLevelSpinner = new javax.swing.JSpinner();
    savePanel = new javax.swing.JPanel();
    saveResults = new javax.swing.JCheckBox();
    resultLabel = new javax.swing.JLabel();
    resultName = new javax.swing.JTextField();

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
      .addGroup(modelEqPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(modelEqScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 751, Short.MAX_VALUE)
        .addContainerGap())
    );
    modelEqPaneLayout.setVerticalGroup(
      modelEqPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(modelEqPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(modelEqScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(13, Short.MAX_VALUE))
    );

    plotBottomPane.setLayout(new java.awt.GridLayout(1, 2, 5, 0));

    distPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Distribution"));

    distButtonGroup.add(normDistButton);
    normDistButton.setSelected(true);
    normDistButton.setText("Normal");
    normDistButton.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));

    distButtonGroup.add(poisDistButton);
    poisDistButton.setText("Poisson");
    poisDistButton.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));

    distButtonGroup.add(binomDistButton);
    binomDistButton.setText("Binomial");
    binomDistButton.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
    binomDistButton.addChangeListener(new javax.swing.event.ChangeListener() {
      public void stateChanged(javax.swing.event.ChangeEvent evt) {
        binomDistButtonStateChanged(evt);
      }
    });

    javax.swing.GroupLayout distPaneLayout = new javax.swing.GroupLayout(distPane);
    distPane.setLayout(distPaneLayout);
    distPaneLayout.setHorizontalGroup(
      distPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(distPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(normDistButton)
        .addGap(18, 18, 18)
        .addComponent(poisDistButton)
        .addGap(18, 18, 18)
        .addComponent(binomDistButton)
        .addContainerGap(59, Short.MAX_VALUE))
    );
    distPaneLayout.setVerticalGroup(
      distPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(distPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(distPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(normDistButton)
          .addComponent(poisDistButton)
          .addComponent(binomDistButton))
        .addContainerGap(16, Short.MAX_VALUE))
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

    sampleSizeLabel.setText("Sample Size:");

    javax.swing.GroupLayout varPaneLayout = new javax.swing.GroupLayout(varPane);
    varPane.setLayout(varPaneLayout);
    varPaneLayout.setHorizontalGroup(
      varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(varPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
          .addComponent(sampleSizeLabel)
          .addComponent(independentLabel)
          .addComponent(dependentLabel))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(independentScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 225, Short.MAX_VALUE)
          .addComponent(sampleSizeComboBox, javax.swing.GroupLayout.Alignment.TRAILING, 0, 225, Short.MAX_VALUE)
          .addComponent(dependentComboBox, javax.swing.GroupLayout.Alignment.TRAILING, 0, 225, Short.MAX_VALUE))
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
          .addComponent(independentScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 266, Short.MAX_VALUE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(sampleSizeComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(sampleSizeLabel))
        .addContainerGap())
    );

    javax.swing.GroupLayout leftPaneLayout = new javax.swing.GroupLayout(leftPane);
    leftPane.setLayout(leftPaneLayout);
    leftPaneLayout.setHorizontalGroup(
      leftPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addComponent(distPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
      .addComponent(varPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
    );
    leftPaneLayout.setVerticalGroup(
      leftPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(leftPaneLayout.createSequentialGroup()
        .addComponent(distPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(varPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    plotBottomPane.add(leftPane);

    plotOptsPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Diagnostic Plots"));

    resVsFit.setText("Residual vs. Fitted");

    normalQQ.setText("Normal Q-Q Plot");

    scaleLoc.setText("Scale-Location Plot");

    cookDist.setText("Cook's Distance Plot");

    influencePlot.setText("Influence Plot");

    javax.swing.GroupLayout plotOptsPaneLayout = new javax.swing.GroupLayout(plotOptsPane);
    plotOptsPane.setLayout(plotOptsPaneLayout);
    plotOptsPaneLayout.setHorizontalGroup(
      plotOptsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(plotOptsPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(plotOptsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(resVsFit)
          .addComponent(normalQQ)
          .addComponent(scaleLoc)
          .addComponent(cookDist)
          .addComponent(influencePlot))
        .addContainerGap(165, Short.MAX_VALUE))
    );
    plotOptsPaneLayout.setVerticalGroup(
      plotOptsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(plotOptsPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(resVsFit)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(normalQQ)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(scaleLoc)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(cookDist)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(influencePlot)
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    analysisOptsPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Analysis Options"));

    rmIntercept.setText("Remove Intercept");

    confIntervals.setText("Compute Confidence Intervals");
    confIntervals.addChangeListener(new javax.swing.event.ChangeListener() {
      public void stateChanged(javax.swing.event.ChangeEvent evt) {
        confIntervalsStateChanged(evt);
      }
    });

    confLabel.setText("Confidence Level:");

    confLevelSpinner.setModel(new javax.swing.SpinnerNumberModel(0.95d, 0.01d, 0.99d, 0.01d));

    javax.swing.GroupLayout analysisOptsPaneLayout = new javax.swing.GroupLayout(analysisOptsPane);
    analysisOptsPane.setLayout(analysisOptsPaneLayout);
    analysisOptsPaneLayout.setHorizontalGroup(
      analysisOptsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(analysisOptsPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(analysisOptsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addGroup(analysisOptsPaneLayout.createSequentialGroup()
            .addComponent(confIntervals)
            .addContainerGap(74, Short.MAX_VALUE))
          .addComponent(rmIntercept)))
      .addGroup(analysisOptsPaneLayout.createSequentialGroup()
        .addGap(59, 59, 59)
        .addComponent(confLabel)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(confLevelSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(55, Short.MAX_VALUE))
    );
    analysisOptsPaneLayout.setVerticalGroup(
      analysisOptsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(analysisOptsPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(rmIntercept)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(confIntervals)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(analysisOptsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(confLabel)
          .addComponent(confLevelSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addContainerGap(13, Short.MAX_VALUE))
    );

    savePanel.setBorder(javax.swing.BorderFactory.createTitledBorder("Results"));

    saveResults.setText("Save R Results?");
    saveResults.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
    saveResults.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        saveResultsActionPerformed(evt);
      }
    });

    resultLabel.setText("Result Name:");
    resultLabel.setEnabled(false);

    resultName.setColumns(12);
    resultName.setEnabled(false);

    javax.swing.GroupLayout savePanelLayout = new javax.swing.GroupLayout(savePanel);
    savePanel.setLayout(savePanelLayout);
    savePanelLayout.setHorizontalGroup(
      savePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(savePanelLayout.createSequentialGroup()
        .addGroup(savePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addGroup(savePanelLayout.createSequentialGroup()
            .addContainerGap()
            .addComponent(saveResults))
          .addGroup(savePanelLayout.createSequentialGroup()
            .addGap(56, 56, 56)
            .addComponent(resultLabel)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(resultName, javax.swing.GroupLayout.PREFERRED_SIZE, 180, javax.swing.GroupLayout.PREFERRED_SIZE)))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    savePanelLayout.setVerticalGroup(
      savePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(savePanelLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(saveResults)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(savePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(resultLabel)
          .addComponent(resultName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    javax.swing.GroupLayout rightPaneLayout = new javax.swing.GroupLayout(rightPane);
    rightPane.setLayout(rightPaneLayout);
    rightPaneLayout.setHorizontalGroup(
      rightPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addComponent(plotOptsPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
      .addComponent(analysisOptsPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
      .addComponent(savePanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
    );
    rightPaneLayout.setVerticalGroup(
      rightPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(rightPaneLayout.createSequentialGroup()
        .addComponent(plotOptsPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(analysisOptsPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(savePanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
    );

    plotBottomPane.add(rightPane);

    javax.swing.GroupLayout plotPaneLayout = new javax.swing.GroupLayout(plotPane);
    plotPane.setLayout(plotPaneLayout);
    plotPaneLayout.setHorizontalGroup(
      plotPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(plotPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(plotPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
          .addComponent(modelEqPane, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
          .addComponent(plotBottomPane, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    plotPaneLayout.setVerticalGroup(
      plotPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(plotPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(modelEqPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(plotBottomPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    setTitle("Linear Regression");

    java.awt.Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
    setBounds((screenSize.width-832)/2, (screenSize.height-828)/2, 832, 828);
  }// </editor-fold>//GEN-END:initComponents

  private void confIntervalsStateChanged(javax.swing.event.ChangeEvent evt)//GEN-FIRST:event_confIntervalsStateChanged
  {//GEN-HEADEREND:event_confIntervalsStateChanged
    this.refreshValidity();
  }//GEN-LAST:event_confIntervalsStateChanged

private void binomDistButtonStateChanged(javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_binomDistButtonStateChanged
  sampleSizeLabel.setEnabled(binomDistButton.isSelected());
  sampleSizeComboBox.setEnabled(binomDistButton.isSelected());
  this.updateModelEquation();
}//GEN-LAST:event_binomDistButtonStateChanged

private void dependentComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dependentComboBoxActionPerformed
  this.updateModelEquation();
}//GEN-LAST:event_dependentComboBoxActionPerformed

private void independentListValueChanged(javax.swing.event.ListSelectionEvent evt) {//GEN-FIRST:event_independentListValueChanged
  this.updateModelEquation();
  this.refreshSubmitButtonValidity();
}//GEN-LAST:event_independentListValueChanged

private void saveResultsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_saveResultsActionPerformed
  resultLabel.setEnabled(saveResults.isSelected());
  resultName.setEnabled(saveResults.isSelected());

  if (saveResults.isSelected())
  {
    resultName.setText("my.glm.fit");
  }
  else
  {
    resultName.setText(null);
  }
}//GEN-LAST:event_saveResultsActionPerformed

  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JPanel analysisOptsPane;
  public javax.swing.JRadioButton binomDistButton;
  private javax.swing.JCheckBox confIntervals;
  private javax.swing.JLabel confLabel;
  private javax.swing.JSpinner confLevelSpinner;
  private javax.swing.JCheckBox cookDist;
  private javax.swing.JComboBox dependentComboBox;
  private javax.swing.JLabel dependentLabel;
  private javax.swing.ButtonGroup distButtonGroup;
  private javax.swing.JPanel distPane;
  private javax.swing.JLabel independentLabel;
  private javax.swing.JList independentList;
  private javax.swing.JScrollPane independentScrollPane;
  private javax.swing.JCheckBox influencePlot;
  private javax.swing.JPanel leftPane;
  private javax.swing.JPanel modelEqPane;
  private javax.swing.JScrollPane modelEqScrollPane;
  private javax.swing.JTextArea modelEqTextArea;
  public javax.swing.JRadioButton normDistButton;
  private javax.swing.JCheckBox normalQQ;
  private javax.swing.JPanel plotBottomPane;
  private javax.swing.JPanel plotOptsPane;
  private javax.swing.JPanel plotPane;
  public javax.swing.JRadioButton poisDistButton;
  private javax.swing.JCheckBox resVsFit;
  private javax.swing.JLabel resultLabel;
  private javax.swing.JTextField resultName;
  private javax.swing.JPanel rightPane;
  private javax.swing.JCheckBox rmIntercept;
  private javax.swing.JComboBox sampleSizeComboBox;
  private javax.swing.JLabel sampleSizeLabel;
  private javax.swing.JPanel savePanel;
  private javax.swing.JCheckBox saveResults;
  private javax.swing.JCheckBox scaleLoc;
  private javax.swing.JPanel varPane;
  // End of variables declaration//GEN-END:variables

  @Override
  protected void submitButtonAction()
  {
    String distFamily = "NULL";

    if (normDistButton.isSelected())
    {
      distFamily = "'gaussian'";
    }
    else if (poisDistButton.isSelected())
    {
      distFamily = "'poisson'";
    }
    else if (binomDistButton.isSelected())
    {
      distFamily = "'binomial'";
    }

    String cmd = "glm.JGR("
      + "my.data=" + (this.getDatasetPane().getDatasetComboBox() != null && this.getDatasetPane().getDatasetComboBox().getSelectedIndex() != -1 ? this.getDatasetPane().getSelectedDataset() : "NULL")
      + ", subset1.name=" + (this.getFactorSelectionPane1().getFactorValueList().isSelectionEmpty() ? "NULL" : RUtils.getStringValue(this.getFactorSelectionPane1().getSelectedFactor()))
      + ", subset1.val=" + (this.getFactorSelectionPane1().getFactorValueList().isSelectionEmpty() ? "NULL" : SubsetFormatter.formatSubset(this.getFactorSelectionPane1().getSelectedFactorValues()))
      + ", subset2.name=" + (this.getFactorSelectionPane2().getFactorValueList().isSelectionEmpty() ? "NULL" : RUtils.getStringValue(this.getFactorSelectionPane2().getSelectedFactor()))
      + ", subset2.val=" + (this.getFactorSelectionPane2().getFactorValueList().isSelectionEmpty() ? "NULL" : SubsetFormatter.formatSubset(this.getFactorSelectionPane2().getSelectedFactorValues()))
      + ", my.formula=" + "'" + this.getModelString() + "'"
      + ", my.family=" + distFamily
      + ", iCI=" + String.valueOf(confIntervals.isSelected()).toUpperCase()
      + (confIntervals.isSelected() ? ", conf.level=" + ((Double) confLevelSpinner.getValue()).doubleValue() : "")
      + ", iDiag.1=" + String.valueOf(resVsFit.isSelected()).toUpperCase()
      + ", iDiag.2=" + String.valueOf(normalQQ.isSelected()).toUpperCase()
      + ", iDiag.3=" + String.valueOf(scaleLoc.isSelected()).toUpperCase()
      + ", iDiag.4=" + String.valueOf(cookDist.isSelected()).toUpperCase()
      + ", iInfluence=" + String.valueOf(influencePlot.isSelected()).toUpperCase()
      + ", iRmIntercept=" + String.valueOf(rmIntercept.isSelected()).toUpperCase()
      + ", saveLMName=" + "'" + resultName.getText() + "'"
      + ")";

    JGR.MAINRCONSOLE.execute(cmd, true);
  }

  @Override
  protected void helpButtonAction()
  {
    String cmd = "CADStat.help('lm.JGR')";
    JGR.MAINRCONSOLE.execute(cmd, true);
    //JGR.MAINRCONSOLE.help("LinearRegression");
  }

  public String getModelString()
  {
    return modelString;
  }

  public void setModelString(String modelString)
  {
    this.modelString = modelString;
  }

  @Override
  public void windowActivated(WindowEvent e)
  {
    Object dep = dependentComboBox.getSelectedItem();
    int[] indepIndices = independentList.getSelectedIndices();

    this.getDatasetPane().refreshDatasetComboBox();
    dependentComboBox.setSelectedItem(dep);
    independentList.setSelectedIndices(indepIndices);

    this.updateModelEquation();

    this.refreshFactorSelectionPanes();
    this.refreshValidity();
  }
}

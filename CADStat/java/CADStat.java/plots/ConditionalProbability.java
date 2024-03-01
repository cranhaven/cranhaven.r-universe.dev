/*
 * ConditionalProbability.java
 *
 * Created on October 9, 2005, 9:47 AM
 */
package org.neptuneinc.cadstat.plots;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.DefaultComboBoxModel;
import javax.swing.SpinnerNumberModel;

import org.neptuneinc.cadstat.prefs.PreferencesManager;
import org.neptuneinc.cadstat.ui.DataPlotDialog;
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
public class ConditionalProbability extends DataPlotDialog
{
  private String condDir;
  private String probDir;

  /**
   * Creates new form ConditionalProbability
   */
  public ConditionalProbability()
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
    this.refreshCondValue();
    this.refreshValidity();
    this.refreshyAxis();
  }

  private void datasetComboBoxActionPerformed(ActionEvent e)
  {
    this.refreshVariableList();
    this.refreshCondValue();
    this.refreshValidity();
    this.refreshyAxis();
  }

  public void refreshVariableListValidity()
  {
    stressorLabel.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && stressorComboBox.getItemCount() > 0);
    stressorComboBox.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && stressorComboBox.getItemCount() > 0);
    responseLabel.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && responseComboBox.getItemCount() > 0);
    responseComboBox.setEnabled(this.getDatasetPane().getDatasetComboBox().getItemCount() > 0 && responseComboBox.getItemCount() > 0);
    weightsLabel.setEnabled(weightsCheckBox.isSelected());
    weightsComboBox.setEnabled(weightsCheckBox.isSelected());
  }

  public void refreshVariableList()
  {
    if (this.getDatasetPane().getDatasetComboBox().getItemCount() > 0)
    {
      Vector<String> nonFactors = null;

      try
      {
        nonFactors = RUtils.nonFactors(this.getDatasetPane().getSelectedDataset());
      }
      catch (REngineException ex)
      {
        Logger.getLogger(ConditionalProbability.class.getName()).log(Level.SEVERE, null, ex);
      }
      catch (REXPMismatchException ex)
      {
        Logger.getLogger(ConditionalProbability.class.getName()).log(Level.SEVERE, null, ex);
      }

      stressorComboBox.setModel(new DefaultComboBoxModel(nonFactors));
      responseComboBox.setModel(new DefaultComboBoxModel(nonFactors));
      weightsComboBox.setModel(new DefaultComboBoxModel(nonFactors));

      if (stressorComboBox.getItemCount() > 0)
      {
        stressorComboBox.setSelectedIndex(0);
      }

      if (responseComboBox.getItemCount() > 0)
      {
        responseComboBox.setSelectedIndex(0);
      }

      if (weightsComboBox.getItemCount() > 0)
      {
        weightsComboBox.setSelectedIndex(0);
      }
    }
    else
    {
      stressorComboBox.removeAllItems();
      responseComboBox.removeAllItems();
      weightsComboBox.removeAllItems();
    }

    this.refreshValidity();
  }

  public void refreshCondValue()
  {
    SpinnerNumberModel model = new SpinnerNumberModel(0, 0, 0, 0);

    if (responseComboBox.getItemCount() > 0)
    {
      String var, classCmd, rangeCmd, meanCmd;

      var = this.getDatasetPane().getSelectedDataset() + "$" + GUIUtils.getSelectedItem(responseComboBox);

      classCmd = "class(" + var + ")";
      rangeCmd = "range(" + var + ", na.rm=T, finite=T)";
      meanCmd = "mean(" + var + ", na.rm=T, finite=T)";

      String className;
      Vector range = null, mean = null;

      try
      {
        className = RUtils.evalAsString(classCmd);

        if (className != null && (className.compareTo("numeric") == 0 || className.compareTo("integer")==0))
        {
          range = RUtils.evalAsVector(rangeCmd);
          mean = RUtils.evalAsVector(meanCmd);

          boolean isRangeValid = range != null;
          boolean isMeanValid = mean != null;

          double def, min, max, step;

          if (isRangeValid)
          {
            min = range.get(0) != null ? Double.parseDouble(range.get(0).toString()) : 0;
            max = range.get(1) != null ? Double.parseDouble(range.get(1).toString()) : 0;

            if (isMeanValid)
            {
              def = Math.rint(Double.parseDouble(mean.get(0).toString()));
            }
            else
            {
              def = 0;
            }

            step = new Double((max - min) / 100.00).intValue();
            step = (step != 0.0) ? step : 1.0;

            model = new SpinnerNumberModel(def, min, max, step);
          }
        }
      }
      catch (REngineException ex)
      {
        Logger.getLogger(ConditionalProbability.class.getName()).log(Level.SEVERE, null, ex);
      }
      catch (REXPMismatchException ex)
      {
        Logger.getLogger(ConditionalProbability.class.getName()).log(Level.SEVERE, null, ex);
      }
    }

    condValueSpinner.setModel(model);
  }

  public void refreshOptionalParamsValidity()
  {
    numBootstrapsLabel.setEnabled(bootstrapCheckBox.isSelected());
    numBootstrapsSpinner.setEnabled(bootstrapCheckBox.isSelected());
  }

  public void refreshyAxis()
  {
    yAxis.setText(GUIUtils.getSelectedItem(responseComboBox));
  }

  public void refreshxAxis()
  {
    xAxis.setText(GUIUtils.getSelectedItem(stressorComboBox));
  }

  public void refreshSubmitButtonValidity()
  {
    this.getSubmitButton().setEnabled(responseComboBox.getItemCount() > 0);
  }

  public void refreshValidity()
  {
    this.refreshVariableListValidity();
    this.refreshOptionalParamsValidity();
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
    mainPane = new javax.swing.JPanel();
    leftPane = new javax.swing.JPanel();
    plotLabelPane = new javax.swing.JPanel();
    plotTitleLabel = new javax.swing.JLabel();
    plotTitle = new javax.swing.JTextField();
    xAxisLabel = new javax.swing.JLabel();
    xAxis = new javax.swing.JTextField();
    yAxisLabel = new javax.swing.JLabel();
    yAxis = new javax.swing.JTextField();
    varPane = new javax.swing.JPanel();
    stressorLabel = new javax.swing.JLabel();
    stressorComboBox = new javax.swing.JComboBox();
    responseLabel = new javax.swing.JLabel();
    responseComboBox = new javax.swing.JComboBox();
    weightsLabel = new javax.swing.JLabel();
    weightsComboBox = new javax.swing.JComboBox();
    weightsCheckBox = new javax.swing.JCheckBox();
    analysisOptionsPane = new javax.swing.JPanel();
    condDirLabel = new javax.swing.JLabel();
    condDirGreaterThanRadioButton = new javax.swing.JRadioButton();
    condDirLessThanRadioButton = new javax.swing.JRadioButton();
    condValueLabel = new javax.swing.JLabel();
    condValueSpinner = new javax.swing.JSpinner();
    probDirLabel = new javax.swing.JLabel();
    probDirGreaterThanEqualRadioButton = new javax.swing.JRadioButton();
    probDirLessThanEqualRadioButton = new javax.swing.JRadioButton();
    bootstrapCheckBox = new javax.swing.JCheckBox();
    alphaLabel = new javax.swing.JLabel();
    alphaSpinner = new javax.swing.JSpinner();
    numBootstrapsLabel = new javax.swing.JLabel();
    numBootstrapsSpinner = new javax.swing.JSpinner();
    condDirButtonGroup = new javax.swing.ButtonGroup();
    probDirButtonGroup = new javax.swing.ButtonGroup();

    mainPane.setLayout(new java.awt.GridLayout(1, 2, 5, 0));

    plotLabelPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Plot Labels"));

    plotTitleLabel.setLabelFor(plotTitle);
    plotTitleLabel.setText("Plot Title:");

    plotTitle.setColumns(12);
    plotTitle.setText("Conditional Probability");

    xAxisLabel.setLabelFor(xAxis);
    xAxisLabel.setText("X-Axis:");

    xAxis.setColumns(12);
    xAxis.setText("X");

    yAxisLabel.setLabelFor(yAxis);
    yAxisLabel.setText("Y-Axis:");

    yAxis.setColumns(12);
    yAxis.setText("Y");

    javax.swing.GroupLayout plotLabelPaneLayout = new javax.swing.GroupLayout(plotLabelPane);
    plotLabelPane.setLayout(plotLabelPaneLayout);
    plotLabelPaneLayout.setHorizontalGroup(
      plotLabelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(plotLabelPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(plotLabelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
          .addComponent(xAxisLabel)
          .addComponent(yAxisLabel)
          .addComponent(plotTitleLabel))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(plotLabelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(xAxis, javax.swing.GroupLayout.DEFAULT_SIZE, 249, Short.MAX_VALUE)
          .addComponent(yAxis, javax.swing.GroupLayout.DEFAULT_SIZE, 249, Short.MAX_VALUE)
          .addComponent(plotTitle, javax.swing.GroupLayout.DEFAULT_SIZE, 249, Short.MAX_VALUE))
        .addContainerGap())
    );
    plotLabelPaneLayout.setVerticalGroup(
      plotLabelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(plotLabelPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(plotLabelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(plotTitleLabel)
          .addComponent(plotTitle, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(plotLabelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(xAxisLabel)
          .addComponent(xAxis, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(plotLabelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(yAxisLabel)
          .addComponent(yAxis, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addContainerGap(25, Short.MAX_VALUE))
    );

    varPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Variables"));

    stressorLabel.setText("Stressor:");

    stressorComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        stressorComboBoxActionPerformed(evt);
      }
    });

    responseLabel.setText("Response:");

    responseComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        responseComboBoxActionPerformed(evt);
      }
    });

    weightsLabel.setText("Weights:");

    weightsComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        weightsComboBoxActionPerformed(evt);
      }
    });

    weightsCheckBox.setText("Specify a weighting variable");
    weightsCheckBox.addChangeListener(new javax.swing.event.ChangeListener() {
      public void stateChanged(javax.swing.event.ChangeEvent evt) {
        weightsCheckBoxStateChanged(evt);
      }
    });

    javax.swing.GroupLayout varPaneLayout = new javax.swing.GroupLayout(varPane);
    varPane.setLayout(varPaneLayout);
    varPaneLayout.setHorizontalGroup(
      varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(varPaneLayout.createSequentialGroup()
        .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addGroup(varPaneLayout.createSequentialGroup()
            .addContainerGap()
            .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
              .addGroup(varPaneLayout.createSequentialGroup()
                .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                  .addComponent(stressorLabel)
                  .addComponent(responseLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                  .addComponent(responseComboBox, 0, 245, Short.MAX_VALUE)
                  .addComponent(stressorComboBox, 0, 245, Short.MAX_VALUE)))
              .addComponent(weightsCheckBox)))
          .addGroup(varPaneLayout.createSequentialGroup()
            .addGap(57, 57, 57)
            .addComponent(weightsLabel)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(weightsComboBox, 0, 212, Short.MAX_VALUE)))
        .addContainerGap())
    );
    varPaneLayout.setVerticalGroup(
      varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(varPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(stressorLabel)
          .addComponent(stressorComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(responseLabel)
          .addComponent(responseComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addGap(18, 18, 18)
        .addComponent(weightsCheckBox)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(weightsLabel)
          .addComponent(weightsComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    javax.swing.GroupLayout leftPaneLayout = new javax.swing.GroupLayout(leftPane);
    leftPane.setLayout(leftPaneLayout);
    leftPaneLayout.setHorizontalGroup(
      leftPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addComponent(varPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
      .addComponent(plotLabelPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
    );
    leftPaneLayout.setVerticalGroup(
      leftPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(leftPaneLayout.createSequentialGroup()
        .addComponent(varPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(plotLabelPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    mainPane.add(leftPane);

    analysisOptionsPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Analysis Options"));

    condDirLabel.setText("Cutoff Value Direction:");

    condDirButtonGroup.add(condDirGreaterThanRadioButton);
    condDirGreaterThanRadioButton.setText("Greater Than");
    condDirGreaterThanRadioButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        condDirGreaterThanRadioButtonActionPerformed(evt);
      }
    });

    condDirButtonGroup.add(condDirLessThanRadioButton);
    condDirLessThanRadioButton.setSelected(true);
    condDirLessThanRadioButton.setText("Less Than");
    condDirLessThanRadioButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        condDirLessThanRadioButtonActionPerformed(evt);
      }
    });

    condValueLabel.setLabelFor(condValueSpinner);
    condValueLabel.setText("Response Cutoff Value:");

    condValueSpinner.setModel(new javax.swing.SpinnerNumberModel(Double.valueOf(0.0d), null, null, Double.valueOf(0.0d)));

    probDirLabel.setText("Probability Direction:");

    probDirButtonGroup.add(probDirGreaterThanEqualRadioButton);
    probDirGreaterThanEqualRadioButton.setSelected(true);
    probDirGreaterThanEqualRadioButton.setText("Greater Than or Equal");
    probDirGreaterThanEqualRadioButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        probDirGreaterThanEqualRadioButtonActionPerformed(evt);
      }
    });

    probDirButtonGroup.add(probDirLessThanEqualRadioButton);
    probDirLessThanEqualRadioButton.setText("Less Than or Equal");
    probDirLessThanEqualRadioButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        probDirLessThanEqualRadioButtonActionPerformed(evt);
      }
    });

    bootstrapCheckBox.setText("Bootstrap confidence intervals");
    bootstrapCheckBox.addChangeListener(new javax.swing.event.ChangeListener() {
      public void stateChanged(javax.swing.event.ChangeEvent evt) {
        bootstrapCheckBoxStateChanged(evt);
      }
    });
    bootstrapCheckBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        bootstrapCheckBoxActionPerformed(evt);
      }
    });

    alphaLabel.setText("Confidence Interval Alpha:");

    alphaSpinner.setModel(new javax.swing.SpinnerNumberModel(0.05d, 0.01d, 1.0d, 0.01d));

    numBootstrapsLabel.setText("Number of Bootstraps:");

    numBootstrapsSpinner.setModel(new javax.swing.SpinnerNumberModel(Integer.valueOf(100), Integer.valueOf(1), null, Integer.valueOf(1)));

    javax.swing.GroupLayout analysisOptionsPaneLayout = new javax.swing.GroupLayout(analysisOptionsPane);
    analysisOptionsPane.setLayout(analysisOptionsPaneLayout);
    analysisOptionsPaneLayout.setHorizontalGroup(
      analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
        .addGroup(analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
            .addContainerGap()
            .addGroup(analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
              .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
                .addComponent(condValueLabel)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(condValueSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE))
              .addComponent(condDirLabel)))
          .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
            .addGap(72, 72, 72)
            .addGroup(analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
              .addComponent(condDirGreaterThanRadioButton)
              .addComponent(condDirLessThanRadioButton)))
          .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
            .addContainerGap()
            .addComponent(probDirLabel))
          .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
            .addGap(75, 75, 75)
            .addGroup(analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
              .addComponent(probDirGreaterThanEqualRadioButton)
              .addComponent(probDirLessThanEqualRadioButton)))
          .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
            .addContainerGap()
            .addComponent(alphaLabel)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(alphaSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE))
          .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
            .addContainerGap()
            .addComponent(bootstrapCheckBox))
          .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
            .addGap(53, 53, 53)
            .addComponent(numBootstrapsLabel)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(numBootstrapsSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, 80, javax.swing.GroupLayout.PREFERRED_SIZE)))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    analysisOptionsPaneLayout.setVerticalGroup(
      analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(condValueLabel)
          .addComponent(condValueSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(condDirLabel)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(condDirGreaterThanRadioButton)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(condDirLessThanRadioButton)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(probDirLabel)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(probDirGreaterThanEqualRadioButton)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(probDirLessThanEqualRadioButton)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(alphaLabel)
          .addComponent(alphaSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(bootstrapCheckBox)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
        .addGroup(analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(numBootstrapsLabel)
          .addComponent(numBootstrapsSpinner, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    mainPane.add(analysisOptionsPane);

    javax.swing.GroupLayout plotPaneLayout = new javax.swing.GroupLayout(plotPane);
    plotPane.setLayout(plotPaneLayout);
    plotPaneLayout.setHorizontalGroup(
      plotPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(plotPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(mainPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    plotPaneLayout.setVerticalGroup(
      plotPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(plotPaneLayout.createSequentialGroup()
        .addComponent(mainPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    setTitle("Conditional Probability");

    java.awt.Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
    setBounds((screenSize.width-810)/2, (screenSize.height-598)/2, 810, 598);
  }// </editor-fold>//GEN-END:initComponents

private void responseComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_responseComboBoxActionPerformed
  this.refreshCondValue();
  this.refreshyAxis();
}//GEN-LAST:event_responseComboBoxActionPerformed

private void bootstrapCheckBoxStateChanged(javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_bootstrapCheckBoxStateChanged
  this.refreshOptionalParamsValidity();
}//GEN-LAST:event_bootstrapCheckBoxStateChanged

private void condDirGreaterThanRadioButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_condDirGreaterThanRadioButtonActionPerformed
  this.setConditionalValueDirection(">");
}//GEN-LAST:event_condDirGreaterThanRadioButtonActionPerformed

private void condDirLessThanRadioButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_condDirLessThanRadioButtonActionPerformed
  this.setConditionalValueDirection("<");
}//GEN-LAST:event_condDirLessThanRadioButtonActionPerformed

private void probDirGreaterThanEqualRadioButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_probDirGreaterThanEqualRadioButtonActionPerformed
  this.setProbabilityDirection(">=");
}//GEN-LAST:event_probDirGreaterThanEqualRadioButtonActionPerformed

private void probDirLessThanEqualRadioButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_probDirLessThanEqualRadioButtonActionPerformed
  this.setProbabilityDirection("<=");
}//GEN-LAST:event_probDirLessThanEqualRadioButtonActionPerformed

private void weightsComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_weightsComboBoxActionPerformed
  // TODO add your handling code here:
}//GEN-LAST:event_weightsComboBoxActionPerformed

private void weightsCheckBoxStateChanged(javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_weightsCheckBoxStateChanged
  this.refreshVariableListValidity();
}//GEN-LAST:event_weightsCheckBoxStateChanged

private void bootstrapCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_bootstrapCheckBoxActionPerformed
  // TODO add your handling code here:
}//GEN-LAST:event_bootstrapCheckBoxActionPerformed

private void stressorComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_stressorComboBoxActionPerformed
  this.refreshxAxis();
}//GEN-LAST:event_stressorComboBoxActionPerformed
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JLabel alphaLabel;
  private javax.swing.JSpinner alphaSpinner;
  private javax.swing.JPanel analysisOptionsPane;
  private javax.swing.JCheckBox bootstrapCheckBox;
  private javax.swing.ButtonGroup condDirButtonGroup;
  private javax.swing.JRadioButton condDirGreaterThanRadioButton;
  private javax.swing.JLabel condDirLabel;
  private javax.swing.JRadioButton condDirLessThanRadioButton;
  private javax.swing.JLabel condValueLabel;
  private javax.swing.JSpinner condValueSpinner;
  private javax.swing.JPanel leftPane;
  private javax.swing.JPanel mainPane;
  private javax.swing.JLabel numBootstrapsLabel;
  private javax.swing.JSpinner numBootstrapsSpinner;
  private javax.swing.JPanel plotLabelPane;
  private javax.swing.JPanel plotPane;
  private javax.swing.JTextField plotTitle;
  private javax.swing.JLabel plotTitleLabel;
  private javax.swing.ButtonGroup probDirButtonGroup;
  private javax.swing.JRadioButton probDirGreaterThanEqualRadioButton;
  private javax.swing.JLabel probDirLabel;
  private javax.swing.JRadioButton probDirLessThanEqualRadioButton;
  private javax.swing.JComboBox responseComboBox;
  private javax.swing.JLabel responseLabel;
  private javax.swing.JComboBox stressorComboBox;
  private javax.swing.JLabel stressorLabel;
  private javax.swing.JPanel varPane;
  private javax.swing.JCheckBox weightsCheckBox;
  private javax.swing.JComboBox weightsComboBox;
  private javax.swing.JLabel weightsLabel;
  private javax.swing.JTextField xAxis;
  private javax.swing.JLabel xAxisLabel;
  private javax.swing.JTextField yAxis;
  private javax.swing.JLabel yAxisLabel;
  // End of variables declaration//GEN-END:variables

  @Override
  protected void submitButtonAction()
  {
    PreferencesManager prefsManager = new PreferencesManager();

    try
    {
      prefsManager.importPreferences();
    }
    catch (Exception ex)
    {
      Logger.getLogger(ConditionalProbability.class.getName()).log(Level.INFO, null, ex);
    }

    String cmd = "conditionalprob.JGR("
      + "my.data=" + this.getDatasetPane().getSelectedDataset()
      + ", x=" + GUIUtils.getSelectedItemR(stressorComboBox)
      + ", y=" + GUIUtils.getSelectedItemR(responseComboBox)
      + ", weights=" + (weightsCheckBox.isSelected() ? GUIUtils.getSelectedItemR(weightsComboBox) : "NULL")
      + ", cond.val=" + condValueSpinner.getValue()
      + ", cond.val.direction=" + (this.getConditionalValueDirection() == null ? "'<'" : RUtils.getStringValue(this.getConditionalValueDirection()))
      + ", prob.direction=" + (this.getProbabilityDirection() == null ? "'>='" : RUtils.getStringValue(this.getProbabilityDirection()))
      + ", alpha=" + alphaSpinner.getValue()
      + ", R=" + (bootstrapCheckBox.isSelected() ? numBootstrapsSpinner.getValue() : "NULL")
      + ", subset1.name=" + (this.getFactorSelectionPane1().getFactorValueList().isSelectionEmpty() ? "NULL" : "'" + this.getFactorSelectionPane1().getSelectedFactor() + "'")
      + ", subset1.val=" + (this.getFactorSelectionPane1().getFactorValueList().isSelectionEmpty() ? "NULL" : SubsetFormatter.formatSubset(this.getFactorSelectionPane1().getSelectedFactorValues()))
      + ", subset2.name=" + (this.getFactorSelectionPane2().getFactorValueList().isSelectionEmpty() ? "NULL" : "'" + this.getFactorSelectionPane2().getSelectedFactor() + "'")
      + ", subset2.val=" + (this.getFactorSelectionPane2().getFactorValueList().isSelectionEmpty() ? "NULL" : SubsetFormatter.formatSubset(this.getFactorSelectionPane2().getSelectedFactorValues()))
      + ", main=" + RUtils.getStringValue(plotTitle.getText())
      + ", xlab=" + RUtils.getStringValue(xAxis.getText())
      + ", ylab=" + RUtils.getStringValue(yAxis.getText())
      + ", " + prefsManager.getCadstatPreferences().toString()
      + ")";
    JGR.MAINRCONSOLE.execute(cmd, true);
  }

  @Override
  protected void helpButtonAction()
  {
    String cmd = "CADStat.help('conditionalprob.JGR')";
    JGR.MAINRCONSOLE.execute(cmd, true);
//    JGR.MAINRCONSOLE.help("Conditionalprob");
  }

  public String getConditionalValueDirection()
  {
    return condDir;
  }

  public void setConditionalValueDirection(String condDir)
  {
    this.condDir = condDir;
  }

  public String getProbabilityDirection()
  {
    return probDir;
  }

  public void setProbabilityDirection(String probDir)
  {
    this.probDir = probDir;
  }

  @Override
  public void windowActivated(WindowEvent e)
  {
    Object s = stressorComboBox.getSelectedItem();
    Object r = responseComboBox.getSelectedItem();
    Object w = weightsComboBox.getSelectedItem();

    this.getDatasetPane().refreshDatasetComboBox();
    this.refreshFactorSelectionPanes();

    stressorComboBox.setSelectedItem(s);
    responseComboBox.setSelectedItem(r);
    weightsComboBox.setSelectedItem(w);

    this.refreshValidity();
  }
}

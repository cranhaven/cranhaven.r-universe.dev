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
import javax.swing.JComboBox;

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
public class RegressionPrediction extends DataPlotDialog
{
  private String modelString;

  /**
   * Creates new form LinearRegression
   */
  public RegressionPrediction()
  {
    super();
  }

  /** Perform custom initialization. */
  @Override
  protected void initCustom()
  {
    this.initComponents();

    getIdVarList().setModel(new DefaultComboBoxModel());
    getIdVarList().insertItemAt("", 0);
    getIdVarList().setSelectedIndex(0);

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
    this.refreshSiteVarList();
    this.refreshIdVarList();
    this.refreshValidity();
    this.updateModelEquation();
  }

  private void datasetComboBoxActionPerformed(ActionEvent e)
  {
    this.refreshVariableList();
    this.refreshSiteVarList();
    this.refreshIdVarList();
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
    boolean datasetValid = this.getDatasetPane().getDatasetComboBox().getItemCount() > 0;

    dependentLabel.setEnabled(datasetValid && dependentComboBox.getItemCount() > 0);
    dependentComboBox.setEnabled(datasetValid && dependentComboBox.getItemCount() > 0);
    independentLabel.setEnabled(datasetValid && independentList.getModel().getSize() > 0);
    independentList.setEnabled(datasetValid && independentList.getModel().getSize() > 0);
    sampleSizeLabel.setEnabled(datasetValid && sampleSizeComboBox.getItemCount() > 0 && binomDistButton.isSelected());
    sampleSizeComboBox.setEnabled(datasetValid && sampleSizeComboBox.getItemCount() > 0 && binomDistButton.isSelected());

    siteVarLabel.setEnabled(datasetValid && siteVarList.getItemCount() > 0);
    siteVarList.setEnabled(datasetValid && siteVarList.getItemCount() > 0);
    siteIDLabel.setEnabled(datasetValid && siteIDValList.getModel().getSize() > 0);
    siteIDValList.setEnabled(datasetValid && siteIDValList.getModel().getSize() > 0);
    idVarLabel.setEnabled(datasetValid && idVarList.getItemCount() > 0);
    idVarList.setEnabled(datasetValid && idVarList.getItemCount() > 0);
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
        Logger.getLogger(RegressionPrediction.class.getName()).log(Level.SEVERE, null, ex);
      }
      catch (REXPMismatchException ex)
      {
        Logger.getLogger(RegressionPrediction.class.getName()).log(Level.SEVERE, null, ex);
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

  private void refreshSiteVarList()
  {
    JComboBox colList = getSiteVarList();

    String dataset = this.getDatasetPane().getSelectedDataset();

    if (dataset != null)
    {
      Vector factors = null;

      try
      {
        factors = RUtils.factors(dataset);
      }
      catch (REngineException ex)
      {
        Logger.getLogger(RegressionPrediction.class.getName()).log(Level.SEVERE, null, ex);
      }
      catch (REXPMismatchException ex)
      {
        Logger.getLogger(RegressionPrediction.class.getName()).log(Level.SEVERE, null, ex);
      }

      colList.removeAllItems();
      colList.setModel(new DefaultComboBoxModel(factors));
    }

    refreshSiteValList();
  }

  private void refreshSiteValList()
  {
    String dataset = this.getDatasetPane().getSelectedDataset();
    Vector v = null;

    if (dataset != null && siteVarList.getItemCount() > 0)
    {
      // Selected column name
      String col = (String) siteVarList.getSelectedItem();

      // Command for unique values
      String cmd = "unique(" + dataset + "$" + col + ")";
      try
      {
        // Evaluate command as list
        v = RUtils.evalAsVector(cmd);

        if (v != null)
        {
          siteIDValList.setListData(v);
          siteIDValList.setSelectedIndex(0);
        }
        else
        {
          siteIDValList.removeAll();
        }
      }
      catch (REngineException ex)
      {
        Logger.getLogger(RegressionPrediction.class.getName()).log(Level.SEVERE, null, ex);
      }
      catch (REXPMismatchException ex)
      {
        Logger.getLogger(RegressionPrediction.class.getName()).log(Level.SEVERE, null, ex);
      }
    }
    else
    {
      siteIDValList.removeAll();
    }

    this.refreshValidity();
  }

  public void refreshSubmitButtonValidity()
  {
    this.getSubmitButton().setEnabled(siteVarList.getSelectedIndex() >= 0 && siteIDValList.getMaxSelectionIndex() >= 0 && !independentList.isSelectionEmpty());
  }

  public void refreshValidity()
  {
    this.refreshModelValidity();
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
    siteSelPane = new javax.swing.JPanel();
    siteVarLabel = new javax.swing.JLabel();
    siteVarList = new javax.swing.JComboBox();
    siteIDLabel = new javax.swing.JLabel();
    siteIDValScrollPane = new javax.swing.JScrollPane();
    siteIDValList = new javax.swing.JList();
    idPane = new javax.swing.JPanel();
    idVarLabel = new javax.swing.JLabel();
    idVarList = new javax.swing.JComboBox();
    analysisOptionsPane = new javax.swing.JPanel();
    sigLabel = new javax.swing.JLabel();
    sigLevel = new javax.swing.JSpinner();
    rmIntercept = new javax.swing.JCheckBox();

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
        .addComponent(modelEqScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 823, Short.MAX_VALUE)
        .addContainerGap())
    );
    modelEqPaneLayout.setVerticalGroup(
      modelEqPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(modelEqPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(modelEqScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

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
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    distPaneLayout.setVerticalGroup(
      distPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(distPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(distPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(normDistButton)
          .addComponent(poisDistButton)
          .addComponent(binomDistButton))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
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
          .addComponent(independentScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 178, Short.MAX_VALUE)
          .addComponent(sampleSizeComboBox, javax.swing.GroupLayout.Alignment.TRAILING, 0, 178, Short.MAX_VALUE)
          .addComponent(dependentComboBox, javax.swing.GroupLayout.Alignment.TRAILING, 0, 178, Short.MAX_VALUE))
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
          .addComponent(independentScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 277, Short.MAX_VALUE))
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

    siteSelPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Reference Selection"));

    siteVarLabel.setText("Reference Variable:");

    siteVarList.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        siteVarListActionPerformed(evt);
      }
    });

    siteIDLabel.setText("Reference Data Values:");

    siteIDValList.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
      public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
        siteIDValListValueChanged(evt);
      }
    });
    siteIDValScrollPane.setViewportView(siteIDValList);

    javax.swing.GroupLayout siteSelPaneLayout = new javax.swing.GroupLayout(siteSelPane);
    siteSelPane.setLayout(siteSelPaneLayout);
    siteSelPaneLayout.setHorizontalGroup(
      siteSelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(siteSelPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(siteSelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
          .addComponent(siteIDLabel)
          .addComponent(siteVarLabel))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(siteSelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(siteIDValScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 254, Short.MAX_VALUE)
          .addComponent(siteVarList, 0, 254, Short.MAX_VALUE))
        .addContainerGap())
    );
    siteSelPaneLayout.setVerticalGroup(
      siteSelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(siteSelPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(siteSelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(siteVarLabel)
          .addComponent(siteVarList, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(siteSelPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(siteIDLabel)
          .addComponent(siteIDValScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 216, Short.MAX_VALUE))
        .addContainerGap())
    );

    idPane.setBorder(javax.swing.BorderFactory.createTitledBorder("ID Variable"));

    idVarLabel.setText("Variable for Labels:");

    javax.swing.GroupLayout idPaneLayout = new javax.swing.GroupLayout(idPane);
    idPane.setLayout(idPaneLayout);
    idPaneLayout.setHorizontalGroup(
      idPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(idPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(idVarLabel)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(idVarList, 0, 291, Short.MAX_VALUE)
        .addContainerGap())
    );
    idPaneLayout.setVerticalGroup(
      idPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(idPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(idPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(idVarLabel)
          .addComponent(idVarList, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    analysisOptionsPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Analysis Options"));

    sigLabel.setText("Significance Level:");

    sigLevel.setModel(new javax.swing.SpinnerNumberModel(0.05d, 0.01d, 0.5d, 0.01d));

    rmIntercept.setText("Remove Intercept");

    javax.swing.GroupLayout analysisOptionsPaneLayout = new javax.swing.GroupLayout(analysisOptionsPane);
    analysisOptionsPane.setLayout(analysisOptionsPaneLayout);
    analysisOptionsPaneLayout.setHorizontalGroup(
      analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(sigLabel)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(sigLevel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addGap(18, 18, 18)
        .addComponent(rmIntercept)
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    analysisOptionsPaneLayout.setVerticalGroup(
      analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(analysisOptionsPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(analysisOptionsPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(sigLabel)
          .addComponent(sigLevel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(rmIntercept))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    javax.swing.GroupLayout rightPaneLayout = new javax.swing.GroupLayout(rightPane);
    rightPane.setLayout(rightPaneLayout);
    rightPaneLayout.setHorizontalGroup(
      rightPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addComponent(siteSelPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
      .addGroup(rightPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
        .addComponent(idPane, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        .addComponent(analysisOptionsPane, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    rightPaneLayout.setVerticalGroup(
      rightPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, rightPaneLayout.createSequentialGroup()
        .addComponent(siteSelPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(idPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(analysisOptionsPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
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
        .addContainerGap()
        .addComponent(modelEqPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(plotPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
          .addComponent(rightPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
          .addComponent(leftPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    setTitle("Regression Prediction");
    getContentPane().setLayout(new javax.swing.BoxLayout(getContentPane(), javax.swing.BoxLayout.Y_AXIS));

    java.awt.Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
    setBounds((screenSize.width-902)/2, (screenSize.height-831)/2, 902, 831);
  }// </editor-fold>//GEN-END:initComponents

  private void siteIDValListValueChanged(javax.swing.event.ListSelectionEvent evt)//GEN-FIRST:event_siteIDValListValueChanged
  {//GEN-HEADEREND:event_siteIDValListValueChanged
    this.refreshValidity();
}//GEN-LAST:event_siteIDValListValueChanged

  private void siteVarListActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_siteVarListActionPerformed
  {//GEN-HEADEREND:event_siteVarListActionPerformed
    refreshSiteValList();
  }//GEN-LAST:event_siteVarListActionPerformed

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

  public javax.swing.JComboBox getIdVarList()
  {
    return idVarList;
  }

  public javax.swing.JComboBox getSiteVarList()
  {
    return siteVarList;
  }

  public Object[] getSiteValListSelectedValues()
  {
    return (GUIUtils.getSelectedValues(siteIDValList));
  }

  private void refreshIdVarList()
  {
    JComboBox colList = getIdVarList();
    for (int k = 1, m = colList.getItemCount(); k < m; ++k)
    {
      colList.removeItemAt(1);
    }
    String dataset = this.getDatasetPane().getSelectedDataset();
    if (dataset != null)
    {
      String[] colnames = null;
      try
      {
        colnames = RUtils.colnames(dataset);

        int colLength = colnames.length;
        for (int k = 0; k < colLength; ++k)
        {
          colList.insertItemAt(colnames[k], k + 1);
        }
        colList.setSelectedIndex(0);
      }
      catch (REngineException ex)
      {
        Logger.getLogger(RegressionPrediction.class.getName()).log(Level.SEVERE, null, ex);
      }
      catch (REXPMismatchException ex)
      {
        Logger.getLogger(RegressionPrediction.class.getName()).log(Level.SEVERE, null, ex);
      }
    }
  }
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JPanel analysisOptionsPane;
  public javax.swing.JRadioButton binomDistButton;
  private javax.swing.JComboBox dependentComboBox;
  private javax.swing.JLabel dependentLabel;
  private javax.swing.ButtonGroup distButtonGroup;
  private javax.swing.JPanel distPane;
  private javax.swing.JPanel idPane;
  private javax.swing.JLabel idVarLabel;
  private javax.swing.JComboBox idVarList;
  private javax.swing.JLabel independentLabel;
  private javax.swing.JList independentList;
  private javax.swing.JScrollPane independentScrollPane;
  private javax.swing.JPanel leftPane;
  private javax.swing.JPanel modelEqPane;
  private javax.swing.JScrollPane modelEqScrollPane;
  private javax.swing.JTextArea modelEqTextArea;
  public javax.swing.JRadioButton normDistButton;
  private javax.swing.JPanel plotPane;
  public javax.swing.JRadioButton poisDistButton;
  private javax.swing.JPanel rightPane;
  private javax.swing.JCheckBox rmIntercept;
  private javax.swing.JComboBox sampleSizeComboBox;
  private javax.swing.JLabel sampleSizeLabel;
  private javax.swing.JLabel sigLabel;
  private javax.swing.JSpinner sigLevel;
  private javax.swing.JLabel siteIDLabel;
  private javax.swing.JList siteIDValList;
  private javax.swing.JScrollPane siteIDValScrollPane;
  private javax.swing.JPanel siteSelPane;
  private javax.swing.JLabel siteVarLabel;
  private javax.swing.JComboBox siteVarList;
  private javax.swing.JPanel varPane;
  // End of variables declaration//GEN-END:variables

  @Override
  protected void submitButtonAction()
  {
    String distfamily = null;

    if (normDistButton.isSelected())
    {
      distfamily = "gaussian";
    }
    else if (poisDistButton.isSelected())
    {
      distfamily = "poisson";
    }
    else if (binomDistButton.isSelected())
    {
      distfamily = "binomial";
    }

    String cmd = "glm.pred.JGR("
      + "my.data=" + this.getDatasetPane().getSelectedDataset()
      + ", subset1.name=" + (this.getFactorSelectionPane1().getFactorValueList().isSelectionEmpty() ? "NULL" : "'" + this.getFactorSelectionPane1().getSelectedFactor() + "'")
      + ", subset1.val=" + (this.getFactorSelectionPane1().getFactorValueList().isSelectionEmpty() ? "NULL" : SubsetFormatter.formatSubset(this.getFactorSelectionPane1().getSelectedFactorValues()))
      + ", subset2.name=" + (this.getFactorSelectionPane2().getFactorValueList().isSelectionEmpty() ? "NULL" : "'" + this.getFactorSelectionPane2().getSelectedFactor() + "'")
      + ", subset2.val=" + (this.getFactorSelectionPane2().getFactorValueList().isSelectionEmpty() ? "NULL" : SubsetFormatter.formatSubset(this.getFactorSelectionPane2().getSelectedFactorValues()))
      + ", site.name=" + "'" + getSiteVarList().getSelectedItem().toString() + "'"
      + ", site.val=" + SubsetFormatter.formatSubset(getSiteValListSelectedValues())
      + ", my.formula=" + "'" + this.getModelString() + "'"
      + ", my.family=" + "'" + distfamily + "'"
      + ", label.name=" + "'" + idVarList.getSelectedItem() + "'"
      + ", sig.level=" + ((Double) sigLevel.getValue()).doubleValue()
      + ", iRmIntercept=" + String.valueOf(rmIntercept.isSelected()).toUpperCase()
      + ")";
    //RUtils.evalAsVector(cmd);
    JGR.MAINRCONSOLE.execute(cmd, true);
  }

  @Override
  protected void helpButtonAction()
  {
    String cmd = "CADStat.help('glm.pred.JGR')";
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
    Object sampleSize = sampleSizeComboBox.getSelectedItem();
    Object refVar = siteVarList.getSelectedItem();
    int[] refValues = siteIDValList.getSelectedIndices();
    Object idVar = idVarList.getSelectedItem();

    this.getDatasetPane().refreshDatasetComboBox();
    this.refreshFactorSelectionPanes();

    dependentComboBox.setSelectedItem(dep);
    independentList.setSelectedIndices(indepIndices);
    sampleSizeComboBox.setSelectedItem(sampleSize);
    siteVarList.setSelectedItem(refVar);
    siteIDValList.setSelectedIndices(refValues);
    idVarList.setSelectedItem(idVar);

    this.updateModelEquation();

    this.refreshValidity();
  }
}

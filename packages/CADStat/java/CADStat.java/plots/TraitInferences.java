/*
 * TraitInferences.java
 *
 * Created on December 22, 2009, 6:00 PM
 */
package org.neptuneinc.cadstat.plots;

import java.util.logging.Level;
import java.util.logging.Logger;
import org.neptuneinc.cadstat.ui.DataPlotDialog;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.util.Vector;
import javax.swing.DefaultComboBoxModel;
import org.neptuneinc.cadstat.utils.GUIUtils;

import org.neptuneinc.cadstat.utils.RUtils;
import org.rosuda.JGR.DataLoader;
import org.rosuda.JGR.JGR;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.REngineException;
import CADStat.java.plots.SubsetFormatter;
 
/**
 *
 * @author  Pasha Minallah
 */
public class TraitInferences extends DataPlotDialog
{
  public TraitInferences()
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
    this.refreshBioInferDatasetList();
    this.refreshValidity();
  }

  private void datasetComboBoxActionPerformed(ActionEvent e)
  {
    this.refreshVariableList();
    this.refreshValidity();
  }

  public void refreshVariableListValidity()
  {
    boolean datasetValid = this.getDatasetPane().getDatasetComboBox().getItemCount() > 0;

    siteLabel.setEnabled(datasetValid && siteComboBox.getItemCount() > 0);
    siteComboBox.setEnabled(datasetValid && siteComboBox.getItemCount() > 0);
    taxonNameLabel.setEnabled(datasetValid && taxonNameComboBox.getItemCount() > 0);
    taxonNameComboBox.setEnabled(datasetValid && taxonNameComboBox.getItemCount() > 0);
    taxonCountLabel.setEnabled(datasetValid && taxonCountComboBox.getItemCount() > 0);
    taxonCountComboBox.setEnabled(datasetValid && taxonCountComboBox.getItemCount() > 0);
  }

  public void refreshVariableList()
  {
    boolean datasetValid = this.getDatasetPane().getDatasetComboBox().getItemCount() > 0;

    if (datasetValid)
    {
      Vector colNames = null;

      try
      {
        colNames = RUtils.colnamesVector(this.getDatasetPane().getSelectedDataset());

        siteComboBox.setModel(new DefaultComboBoxModel(colNames));
        taxonNameComboBox.setModel(new DefaultComboBoxModel(colNames));
        taxonCountComboBox.setModel(new DefaultComboBoxModel(colNames));

        if (siteComboBox.getItemCount() > 0)
        {
          siteComboBox.setSelectedIndex(0);
        }

        if (taxonNameComboBox.getItemCount() > 0)
        {
          taxonNameComboBox.setSelectedIndex(0);
        }

        if (taxonCountComboBox.getItemCount() > 0)
        {
          taxonCountComboBox.setSelectedIndex(0);
        }
      }

      catch (REngineException ex)
      {
        Logger.getLogger(TraitInferences.class.getName()).log(Level.SEVERE, null, ex);
      }      catch (REXPMismatchException ex)
      {
        Logger.getLogger(TraitInferences.class.getName()).log(Level.SEVERE, null, ex);
      }
    }
    else
    {
      siteComboBox.removeAllItems();
      taxonNameComboBox.removeAllItems();
      taxonCountComboBox.removeAllItems();
    }

    this.refreshValidity();
  }

  public void refreshBioInferDatasetList()
  {
    String cmd = "library(bio.infer); flist.match('^trait');";

    Object currentDataset = coefBioInferDataComboBox.getSelectedItem();
    String[] datasets = null;

    try
    {
      datasets = RUtils.R_ENGINE.parseAndEval(cmd).asStrings();

      if (datasets != null && datasets.length > 0)
      {
        coefBioInferDataComboBox.setModel(new DefaultComboBoxModel(datasets));
      }
      else
      {
        coefBioInferDataComboBox.removeAllItems();
      }

      if (coefBioInferDataComboBox.getModel().getSize() > 0)
      {
        if (currentDataset != null)
        {
          coefBioInferDataComboBox.setSelectedItem(currentDataset);
        }
        else
        {
          coefBioInferDataComboBox.setSelectedIndex(0);
        }
      }
    }

    catch (REngineException ex)
    {
      Logger.getLogger(TraitInferences.class.getName()).log(Level.SEVERE, null, ex);
    }    catch (REXPMismatchException ex)
    {
      Logger.getLogger(TraitInferences.class.getName()).log(Level.SEVERE, null, ex);
    }

    this.refreshCoeffValidity();
  }

  public void refreshCoeffValidity()
  {
    boolean biValid = coefBioInferDataRadioButton.isSelected();
    //boolean regValid = coefRegDataRadioButton.isSelected();

    coefBioInferDataLabel.setEnabled(biValid);
    coefBioInferDataComboBox.setEnabled(biValid);

    //coefRegDataLabel.setEnabled(regValid);
    //coefRegDataComboBox.setEnabled(regValid && coefRegDataComboBox.getModel().getSize() > 0);
    //coefRegDataBrowseButton.setEnabled(regValid);
  }

  public void refreshSubmitButtonValidity()
  {
    this.getSubmitButton().setEnabled(this.getDatasetPane().getDatasetComboBox().getSelectedIndex() != -1
      && siteComboBox.getSelectedIndex() != -1
      && taxonNameComboBox.getSelectedIndex() != -1
      && taxonCountComboBox.getSelectedIndex() != -1
      && (coefBioInferDataComboBox.getSelectedIndex() != -1));// || coefRegDataComboBox.getSelectedIndex() != -1));
  }

  public void refreshValidity()
  {
    this.refreshVariableListValidity();
    this.refreshCoeffValidity();
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
        varPane = new javax.swing.JPanel();
        siteLabel = new javax.swing.JLabel();
        siteComboBox = new javax.swing.JComboBox();
        taxonNameLabel = new javax.swing.JLabel();
        taxonNameComboBox = new javax.swing.JComboBox();
        taxonCountLabel = new javax.swing.JLabel();
        taxonCountComboBox = new javax.swing.JComboBox();
        coefPane = new javax.swing.JPanel();
        coefBioInferDataRadioButton = new javax.swing.JRadioButton();
        coefBioInferDataLabel = new javax.swing.JLabel();
        coefBioInferDataComboBox = new javax.swing.JComboBox();
        savePanel = new javax.swing.JPanel();
        saveResults = new javax.swing.JCheckBox();
        resultLabel = new javax.swing.JLabel();
        resultName = new javax.swing.JTextField();
        coefDatabuttonGroup = new javax.swing.ButtonGroup();

        varPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Variables"));

        siteLabel.setText("Site ID:");

        siteComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                siteComboBoxActionPerformed(evt);
            }
        });

        taxonNameLabel.setText("Taxon Name:");

        taxonNameComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                taxonNameComboBoxActionPerformed(evt);
            }
        });

        taxonCountLabel.setText("Taxon Count:");

        taxonCountComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                taxonCountComboBoxActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout varPaneLayout = new javax.swing.GroupLayout(varPane);
        varPane.setLayout(varPaneLayout);
        varPaneLayout.setHorizontalGroup(
            varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(varPaneLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(taxonCountLabel)
                    .addComponent(taxonNameLabel)
                    .addComponent(siteLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(taxonCountComboBox, javax.swing.GroupLayout.Alignment.TRAILING, 0, 424, Short.MAX_VALUE)
                    .addComponent(siteComboBox, javax.swing.GroupLayout.Alignment.TRAILING, 0, 424, Short.MAX_VALUE)
                    .addComponent(taxonNameComboBox, 0, 424, Short.MAX_VALUE))
                .addContainerGap())
        );
        varPaneLayout.setVerticalGroup(
            varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(varPaneLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(siteComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(siteLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(taxonNameLabel)
                    .addComponent(taxonNameComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 8, Short.MAX_VALUE)
                .addGroup(varPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(taxonCountComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(taxonCountLabel))
                .addContainerGap())
        );

        coefPane.setBorder(javax.swing.BorderFactory.createTitledBorder("Trait Data"));
        coefPane.setPreferredSize(new java.awt.Dimension(526, 91));

        coefDatabuttonGroup.add(coefBioInferDataRadioButton);
        coefBioInferDataRadioButton.setSelected(true);
        coefBioInferDataRadioButton.setText("Select a trait dataset from the bio.infer package:");
        coefBioInferDataRadioButton.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                coefBioInferDataRadioButtonStateChanged(evt);
            }
        });
        coefBioInferDataRadioButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                coefBioInferDataRadioButtonActionPerformed(evt);
            }
        });

        coefBioInferDataLabel.setText("bio.infer trait dataset:");

        coefBioInferDataComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                coefBioInferDataComboBoxActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout coefPaneLayout = new javax.swing.GroupLayout(coefPane);
        coefPane.setLayout(coefPaneLayout);
        coefPaneLayout.setHorizontalGroup(
            coefPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(coefPaneLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(coefPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(coefBioInferDataRadioButton)
                    .addGroup(coefPaneLayout.createSequentialGroup()
                        .addComponent(coefBioInferDataLabel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(coefBioInferDataComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, 219, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap(174, Short.MAX_VALUE))
        );
        coefPaneLayout.setVerticalGroup(
            coefPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(coefPaneLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(coefBioInferDataRadioButton)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(coefPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(coefBioInferDataComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(coefBioInferDataLabel))
                .addContainerGap(12, Short.MAX_VALUE))
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
        resultName.setText("traitResult");
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
                        .addComponent(resultName, javax.swing.GroupLayout.DEFAULT_SIZE, 380, Short.MAX_VALUE)))
                .addContainerGap())
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

        javax.swing.GroupLayout mainPaneLayout = new javax.swing.GroupLayout(mainPane);
        mainPane.setLayout(mainPaneLayout);
        mainPaneLayout.setHorizontalGroup(
            mainPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(coefPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(varPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(savePanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        mainPaneLayout.setVerticalGroup(
            mainPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, mainPaneLayout.createSequentialGroup()
                .addComponent(varPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(coefPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(savePanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        plotPane.add(mainPane);

        setTitle("Calculate Trait Matrix");

        java.awt.Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        setBounds((screenSize.width-680)/2, (screenSize.height-708)/2, 680, 708);
    }// </editor-fold>//GEN-END:initComponents

private void saveResultsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_saveResultsActionPerformed
  resultLabel.setEnabled(saveResults.isSelected());
  resultName.setEnabled(saveResults.isSelected());
}//GEN-LAST:event_saveResultsActionPerformed

private void siteComboBoxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_siteComboBoxActionPerformed
{//GEN-HEADEREND:event_siteComboBoxActionPerformed
  this.refreshSubmitButtonValidity();
}//GEN-LAST:event_siteComboBoxActionPerformed

private void taxonNameComboBoxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_taxonNameComboBoxActionPerformed
{//GEN-HEADEREND:event_taxonNameComboBoxActionPerformed
  this.refreshSubmitButtonValidity();
}//GEN-LAST:event_taxonNameComboBoxActionPerformed

private void taxonCountComboBoxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_taxonCountComboBoxActionPerformed
{//GEN-HEADEREND:event_taxonCountComboBoxActionPerformed
  this.refreshSubmitButtonValidity();
}//GEN-LAST:event_taxonCountComboBoxActionPerformed

private void coefBioInferDataRadioButtonStateChanged(javax.swing.event.ChangeEvent evt)//GEN-FIRST:event_coefBioInferDataRadioButtonStateChanged
{//GEN-HEADEREND:event_coefBioInferDataRadioButtonStateChanged
  refreshCoeffValidity();
}//GEN-LAST:event_coefBioInferDataRadioButtonStateChanged

private void coefBioInferDataComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_coefBioInferDataComboBoxActionPerformed
    // TODO add your handling code here:
}//GEN-LAST:event_coefBioInferDataComboBoxActionPerformed

private void coefBioInferDataRadioButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_coefBioInferDataRadioButtonActionPerformed
    // TODO add your handling code here:
}//GEN-LAST:event_coefBioInferDataRadioButtonActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JComboBox coefBioInferDataComboBox;
    private javax.swing.JLabel coefBioInferDataLabel;
    private javax.swing.JRadioButton coefBioInferDataRadioButton;
    private javax.swing.ButtonGroup coefDatabuttonGroup;
    private javax.swing.JPanel coefPane;
    private javax.swing.JPanel mainPane;
    private javax.swing.JPanel plotPane;
    private javax.swing.JLabel resultLabel;
    private javax.swing.JTextField resultName;
    private javax.swing.JPanel savePanel;
    private javax.swing.JCheckBox saveResults;
    private javax.swing.JComboBox siteComboBox;
    private javax.swing.JLabel siteLabel;
    private javax.swing.JComboBox taxonCountComboBox;
    private javax.swing.JLabel taxonCountLabel;
    private javax.swing.JComboBox taxonNameComboBox;
    private javax.swing.JLabel taxonNameLabel;
    private javax.swing.JPanel varPane;
    // End of variables declaration//GEN-END:variables

  @Override
  protected void submitButtonAction()
  {
    String cmd = "bioinfer.JGR("
      + "bio.data=" + this.getDatasetPane().getSelectedDataset()
      + ", subset1.name=" + (this.getFactorSelectionPane1().getFactorValueList().isSelectionEmpty() ? "NULL" : "'" + this.getFactorSelectionPane1().getSelectedFactor() + "'")
      + ", subset1.val=" + (this.getFactorSelectionPane1().getFactorValueList().isSelectionEmpty() ? "NULL" : SubsetFormatter.formatSubset(this.getFactorSelectionPane1().getSelectedFactorValues()))
      + ", subset2.name=" + (this.getFactorSelectionPane2().getFactorValueList().isSelectionEmpty() ? "NULL" : "'" + this.getFactorSelectionPane2().getSelectedFactor() + "'")
      + ", subset2.val=" + (this.getFactorSelectionPane2().getFactorValueList().isSelectionEmpty() ? "NULL" : SubsetFormatter.formatSubset(this.getFactorSelectionPane2().getSelectedFactorValues()))
      + ", siteID=" + GUIUtils.getSelectedItemR(siteComboBox)
      + ", taxonName=" + GUIUtils.getSelectedItemR(taxonNameComboBox)
      + ", taxonCount=" + GUIUtils.getSelectedItemR(taxonCountComboBox)
      + ", coefBioInferData=" + GUIUtils.getBooleanValueR(coefBioInferDataRadioButton)
      + ", coefBioInferDataName=" + GUIUtils.getSelectedItemR(coefBioInferDataComboBox)
//      + ", coefRegData=" + GUIUtils.getBooleanValueR(coefRegDataRadioButton)
//      + ", coefRegDataName=" + GUIUtils.getSelectedItemR(coefRegDataComboBox)
      + ", saveResults=" + GUIUtils.getBooleanValueR(saveResults)
      + ", resultName=" + RUtils.getStringValue(resultName.getText())
      + ", tname.new=NULL"
      + ", dupe.sel=NULL"
      + ", pecboORtrait='trait'"
      + ")";

    JGR.MAINRCONSOLE.execute(cmd, true);
  }

  @Override
  protected void helpButtonAction()
  {
    String cmd = "CADStat.help('trait.stat.JGR')";
    JGR.MAINRCONSOLE.execute(cmd, true);
  }

  @Override
  public void windowActivated(WindowEvent e)
  {
    Object site = siteComboBox.getSelectedItem();
    Object tName = taxonNameComboBox.getSelectedItem();
    Object tCount = taxonCountComboBox.getSelectedItem();

    this.getDatasetPane().refreshDatasetComboBox();
    this.refreshBioInferDatasetList();
    //GUIUtils.refreshDataset(coefRegDataComboBox);

    siteComboBox.setSelectedItem(site);
    taxonNameComboBox.setSelectedItem(tName);
    taxonCountComboBox.setSelectedItem(tCount);

    this.refreshFactorSelectionPanes();
    this.refreshValidity();
  }
}

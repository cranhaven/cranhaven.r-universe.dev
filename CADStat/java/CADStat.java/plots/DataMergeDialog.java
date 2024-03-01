/*
 * DataMergeDialog.java
 *
 * Created on September 20, 2005, 8:05 PM
 */
package org.neptuneinc.cadstat.plots;

import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFileChooser;
import javax.swing.JTextField;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import org.neptuneinc.cadstat.utils.GUIUtils;

import org.neptuneinc.cadstat.utils.RUtils;
import org.rosuda.JGR.DataLoader;
import org.rosuda.JGR.JGR;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.REngineException;

/**
 *
 * @author Mark Fitzgerald
 * @author Pasha Minallah
 */
public class DataMergeDialog extends JDialog implements WindowListener
{
  private Map<JComboBox,List<JComboBox>> datasetVariableMap;

  public DataMergeDialog()
  {
    this.initComponents();

    this.getRootPane().setDefaultButton(submitButton);

    this.addWindowListener(this);

    this.initCustom();
    this.refreshDatasetLists();
    this.refreshVariableLists();

    refreshValidity();

    pack();
  }

  private void initCustom()
  {
    this.setDatasetVariableMap(new HashMap<JComboBox,List<JComboBox>>(2));

    this.getDatasetVariableMap().put(this.getDataset1ComboBox(), new ArrayList<JComboBox>(3));
    this.getDatasetVariableMap().put(this.getDataset2ComboBox(), new ArrayList<JComboBox>(3));

    this.getDatasetVariableMap().get(this.getDataset1ComboBox()).add(this.getBy11ComboBox());
    this.getDatasetVariableMap().get(this.getDataset1ComboBox()).add(this.getBy12ComboBox());
    this.getDatasetVariableMap().get(this.getDataset1ComboBox()).add(this.getBy13ComboBox());
    this.getDatasetVariableMap().get(this.getDataset2ComboBox()).add(this.getBy21ComboBox());
    this.getDatasetVariableMap().get(this.getDataset2ComboBox()).add(this.getBy22ComboBox());
    this.getDatasetVariableMap().get(this.getDataset2ComboBox()).add(this.getBy23ComboBox());

    // Initialize dataset combo boxes
    for (JComboBox b : this.getDatasetVariableMap().keySet())
    {
      b.setModel(new DefaultComboBoxModel());
    }

    // Initialize variable combo boxes
    for (List<JComboBox> l : this.getDatasetVariableMap().values())
    {
      for (JComboBox b : l)
      {
        b.setModel(new DefaultComboBoxModel());
      }
    }
    
  }

  private void refreshValidity()
  {
    this.refreshDatasetValidity();
    this.refreshSubmitButtonValidity();
  }

  public void refreshDatasetValidity()
  {
    for (JComboBox datasetComboBox : this.getDatasetVariableMap().keySet())
    {
      if (datasetComboBox != null)
      {
        datasetComboBox.setEnabled(datasetComboBox.getModel().getSize() > 0);
      }
    }
  }

  // Checks to see if a dropdown box is blank.
  private boolean isNotBlank(JComboBox variable){
      return variable != null && variable.getSelectedIndex() != -1;
  }
  
  public void refreshSubmitButtonValidity()
  {
    boolean valid = true;
       
    // Checks to see if two datasets have been loaded.
    for (JComboBox datasetComboBox : this.getDatasetVariableMap().keySet())
    {
      valid = valid && (datasetComboBox != null && datasetComboBox.getSelectedIndex() != -1);
    }

    List<JComboBox> dataset1 = this.getDatasetVariableMap().get(this.getDataset1ComboBox());
    List<JComboBox> dataset2 = this.getDatasetVariableMap().get(this.getDataset2ComboBox());
    
    int NumVariablesPerDataset = 3;
    boolean hasOneByVariable = false;
    
    
    for (int i=0; i<NumVariablesPerDataset; i++){
        // Check if there's one by-variable to merge.
        hasOneByVariable = hasOneByVariable || (isNotBlank(dataset1.get(i)) && isNotBlank(dataset2.get(i)));

        // Check if a pair in a given row matches or not.        
        valid = valid && (!(isNotBlank(dataset1.get(i)) || isNotBlank(dataset2.get(i)))||isNotBlank(dataset1.get(i)) && isNotBlank(dataset2.get(i)));           
    }
    
    // Ensures that there is at least one by-variable to sort with.
    valid = valid && hasOneByVariable;
    
    submitButton.setEnabled(valid);
  }

  public void refreshDatasetLists()
  {
    Object var11 = this.getBy11ComboBox().getSelectedItem();
    Object var12 = this.getBy12ComboBox().getSelectedItem();
    Object var13 = this.getBy13ComboBox().getSelectedItem();

    Object var21 = this.getBy21ComboBox().getSelectedItem();
    Object var22 = this.getBy22ComboBox().getSelectedItem();
    Object var23 = this.getBy23ComboBox().getSelectedItem();
    
    try
    {
      Object[] datasets = RUtils.getDatasetList();

      for (JComboBox datasetComboBox : this.getDatasetVariableMap().keySet())
      {
        GUIUtils.updateDataList(datasetComboBox, datasets);
      }
    }
    catch (REngineException ex)
    {
      Logger.getLogger(DataMergeDialog.class.getName()).log(Level.SEVERE, null, ex);
    }
    catch (REXPMismatchException ex)
    {
      Logger.getLogger(DataMergeDialog.class.getName()).log(Level.SEVERE, null, ex);
    }

    this.getBy11ComboBox().setSelectedItem(var11);
    this.getBy12ComboBox().setSelectedItem(var12);
    this.getBy13ComboBox().setSelectedItem(var13);

    this.getBy21ComboBox().setSelectedItem(var21);
    this.getBy22ComboBox().setSelectedItem(var22);
    this.getBy23ComboBox().setSelectedItem(var23);

    refreshValidity();
  }

  public void refreshVariableLists()
  {
    for (JComboBox datasetComboBox : this.getDatasetVariableMap().keySet())
    {
      this.refreshVariableLists(datasetComboBox);
    }

    refreshValidity();
  }

  public void refreshVariableLists(JComboBox datasetComboBox)
  {
    if (datasetComboBox != null && datasetComboBox.getSelectedIndex() != -1)
    {
      String datasetName = datasetComboBox.getSelectedItem().toString();

      String[] colnames = null;

      try
      {
        colnames = RUtils.colnames(datasetName);

        for (JComboBox varList : this.getDatasetVariableMap().get(datasetComboBox))
        {
          if (colnames != null && colnames.length > 0)
          {
            Object selectedItem = varList.getSelectedItem();
            varList.setModel(new DefaultComboBoxModel(colnames));
            varList.setSelectedItem(selectedItem);
          }
          else
          {
            varList.removeAllItems();
          }
        }
      }
      catch (REngineException ex)
      {
        Logger.getLogger(DataMergeDialog.class.getName()).log(Level.SEVERE, null, ex);
      }
      catch (REXPMismatchException ex)
      {
        Logger.getLogger(DataMergeDialog.class.getName()).log(Level.SEVERE, null, ex);
      }
    }
  }

  public javax.swing.JPanel getDataset1Pane()
  {
    return dataset1Pane;
  }

  public void setDataset1Pane(javax.swing.JPanel dataset1Pane)
  {
    this.dataset1Pane = dataset1Pane;
  }

  public javax.swing.JPanel getDataset2Pane()
  {
    return dataset1Pane;
  }

  public void setDataset2Pane(javax.swing.JPanel dataset2Pane)
  {
    this.dataset2Pane = dataset2Pane;
  }

  public javax.swing.JPanel getSavePane()
  {
    return savePane;
  }

  public void setSavePane(javax.swing.JPanel savePane)
  {
    this.savePane = savePane;
  }

  public javax.swing.JComboBox getDataset1ComboBox()
  {
    return dataset1ComboBox;
  }

  public javax.swing.JComboBox getDataset2ComboBox()
  {
    return dataset2ComboBox;
  }

  public javax.swing.JComboBox getBy11ComboBox()
  {
    return by11ComboBox;
  }

  public javax.swing.JComboBox getBy12ComboBox()
  {
    return by12ComboBox;
  }

  public javax.swing.JComboBox getBy13ComboBox()
  {
    return by13ComboBox;
  }

  public javax.swing.JComboBox getBy21ComboBox()
  {
    return by21ComboBox;
  }

  public javax.swing.JComboBox getBy22ComboBox()
  {
    return by22ComboBox;
  }

  public javax.swing.JComboBox getBy23ComboBox()
  {
    return by23ComboBox;
  }

  private int setTextUsingFileChooser(JTextField textField, int mode)
  {
    String workingDir = null;

    try
    {
      workingDir = RUtils.evalAsString("getwd()");
    }
    catch (REngineException ex)
    {
      Logger.getLogger(DataMergeDialog.class.getName()).log(Level.SEVERE, null, ex);
    }
    catch (REXPMismatchException ex)
    {
      Logger.getLogger(DataMergeDialog.class.getName()).log(Level.SEVERE, null, ex);
    }

    JFileChooser fc = new JFileChooser(workingDir);
    fc.setMultiSelectionEnabled(false);
    fc.setFileSelectionMode(mode);

    int fcReturnState = fc.showOpenDialog(this);

    if (fcReturnState == JFileChooser.APPROVE_OPTION)
    {
      textField.setText(fc.getSelectedFile().toString().replace('\\', '/'));
    }

    return fcReturnState;
  }

  /** This method is called from within the constructor to
   * initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is
   * always regenerated by the Form Editor.
   */
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {

    datasetPane = new javax.swing.JPanel();
    dataset1Pane = new javax.swing.JPanel();
    dataset1Label = new javax.swing.JLabel();
    dataset1ComboBox = new javax.swing.JComboBox();
    browse1Button = new javax.swing.JButton();
    by11Label = new javax.swing.JLabel();
    by11ComboBox = new javax.swing.JComboBox();
    by12Label = new javax.swing.JLabel();
    by12ComboBox = new javax.swing.JComboBox();
    by13Label = new javax.swing.JLabel();
    by13ComboBox = new javax.swing.JComboBox();
    byall1CheckBox = new javax.swing.JCheckBox();
    suffix1Label = new javax.swing.JLabel();
    suffix1 = new javax.swing.JTextField();
    dataset2Pane = new javax.swing.JPanel();
    dataset2Label = new javax.swing.JLabel();
    dataset2ComboBox = new javax.swing.JComboBox();
    browse2Button = new javax.swing.JButton();
    by21Label = new javax.swing.JLabel();
    by21ComboBox = new javax.swing.JComboBox();
    by22Label = new javax.swing.JLabel();
    by22ComboBox = new javax.swing.JComboBox();
    by23Label = new javax.swing.JLabel();
    by23ComboBox = new javax.swing.JComboBox();
    byall2CheckBox = new javax.swing.JCheckBox();
    suffix2Label = new javax.swing.JLabel();
    suffix2 = new javax.swing.JTextField();
    savePane = new javax.swing.JPanel();
    saveLeftPane = new javax.swing.JPanel();
    sortCheck = new javax.swing.JCheckBox();
    rSaveLabel = new javax.swing.JLabel();
    rNameText = new javax.swing.JTextField();
    saveRightPane = new javax.swing.JPanel();
    saveCheck = new javax.swing.JCheckBox();
    saveLabel = new javax.swing.JLabel();
    nameText = new javax.swing.JTextField();
    outputBrowse = new javax.swing.JButton();
    buttonPane = new javax.swing.JPanel();
    cancelButton = new javax.swing.JButton();
    submitButton = new javax.swing.JButton();
    helpButton = new javax.swing.JButton();

    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
    setTitle("Data Merge");
    setMinimumSize(new java.awt.Dimension(30, 30));

    datasetPane.setLayout(new java.awt.GridLayout(1, 2, 5, 0));

    dataset1Pane.setBorder(javax.swing.BorderFactory.createTitledBorder("Dataset 1"));
    dataset1Pane.setMinimumSize(new java.awt.Dimension(30, 30));

    dataset1Label.setLabelFor(dataset1ComboBox);
    dataset1Label.setText("Dataset 1:");

    dataset1ComboBox.setMinimumSize(new java.awt.Dimension(120, 24));
    dataset1ComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        dataset1ComboBoxActionPerformed(evt);
      }
    });

    browse1Button.setText("Browse...");
    browse1Button.setMargin(new java.awt.Insets(0, 5, 0, 5));
    browse1Button.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        browse1ButtonActionPerformed(evt);
      }
    });

    by11Label.setLabelFor(by11ComboBox);
    by11Label.setText("Variable 1:");
    by11Label.setToolTipText("Pick variable from Dataset 1 that corresponds to Variable 1 in Dataset 2.  Necessary if column names do not match.");

    by11ComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        by11ComboBoxActionPerformed(evt);
      }
    });

    by12Label.setLabelFor(by12ComboBox);
    by12Label.setText("Variable 2:");
    by12Label.setToolTipText("Pick variable from Dataset 1 that corresponds to Variable 2 in Dataset 2.  Necessary if column names do not match.");

    by12ComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        by12ComboBoxActionPerformed(evt);
      }
    });

    by13Label.setLabelFor(by13ComboBox);
    by13Label.setText("Variable 3:");
    by13Label.setToolTipText("Pick variable from Dataset 1 that corresponds to Variable 3 in Dataset 2.  Necessary if column names do not match.");

    by13ComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        by13ComboBoxActionPerformed(evt);
      }
    });

    byall1CheckBox.setSelected(true);
    byall1CheckBox.setText("By All");
    byall1CheckBox.setToolTipText("If selected, then all rows in Dataset 1 will be included, even if there is no corresponding row in Dataset 2.");
    byall1CheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));

    suffix1Label.setText("Suffix: ");
    suffix1Label.setToolTipText("If column names match in Dataset 1 and 2, then this suffix will be added to the column name for Dataset 1.");

    suffix1.setText(".1");

    javax.swing.GroupLayout dataset1PaneLayout = new javax.swing.GroupLayout(dataset1Pane);
    dataset1Pane.setLayout(dataset1PaneLayout);
    dataset1PaneLayout.setHorizontalGroup(
      dataset1PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(dataset1PaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(dataset1PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addGroup(dataset1PaneLayout.createSequentialGroup()
            .addComponent(dataset1Label)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(dataset1ComboBox, 0, 230, Short.MAX_VALUE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(browse1Button))
          .addGroup(dataset1PaneLayout.createSequentialGroup()
            .addGap(12, 12, 12)
            .addGroup(dataset1PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
              .addComponent(by12Label)
              .addComponent(by11Label)
              .addComponent(by13Label)
              .addComponent(suffix1Label))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(dataset1PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
              .addComponent(by12ComboBox, 0, 325, Short.MAX_VALUE)
              .addComponent(by11ComboBox, 0, 325, Short.MAX_VALUE)
              .addComponent(by13ComboBox, 0, 325, Short.MAX_VALUE)
              .addComponent(byall1CheckBox)
              .addComponent(suffix1, javax.swing.GroupLayout.PREFERRED_SIZE, 80, javax.swing.GroupLayout.PREFERRED_SIZE))))
        .addContainerGap())
    );
    dataset1PaneLayout.setVerticalGroup(
      dataset1PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(dataset1PaneLayout.createSequentialGroup()
        .addGroup(dataset1PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(dataset1Label)
          .addComponent(dataset1ComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(browse1Button))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(dataset1PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(by11Label)
          .addComponent(by11ComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(dataset1PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(by12Label)
          .addComponent(by12ComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(dataset1PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(by13Label)
          .addComponent(by13ComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(byall1CheckBox)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(dataset1PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(suffix1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(suffix1Label))
        .addContainerGap())
    );

    datasetPane.add(dataset1Pane);

    dataset2Pane.setBorder(javax.swing.BorderFactory.createTitledBorder("Dataset 2"));
    dataset2Pane.setMinimumSize(new java.awt.Dimension(30, 30));

    dataset2Label.setText("Dataset 2:");

    dataset2ComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        dataset2ComboBoxActionPerformed(evt);
      }
    });

    browse2Button.setText("Browse...");
    browse2Button.setMargin(new java.awt.Insets(0, 5, 0, 5));
    browse2Button.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        browse2ButtonActionPerformed(evt);
      }
    });

    by21Label.setLabelFor(by21ComboBox);
    by21Label.setText("Variable 1:");
    by21Label.setToolTipText("Pick variable from Dataset 2 that corresponds to Variable 1 in Dataset 1.  Necessary if column names do not match.");

    by21ComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        by21ComboBoxActionPerformed(evt);
      }
    });

    by22Label.setLabelFor(by22ComboBox);
    by22Label.setText("Variable 2:");
    by22Label.setToolTipText("Pick Variable from Dataset 2 that corresponds to Variable2 from Dataset 1.  Necessary if column names do not match.");

    by22ComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        by22ComboBoxActionPerformed(evt);
      }
    });

    by23Label.setLabelFor(by23ComboBox);
    by23Label.setText("Variable 3:");
    by23Label.setToolTipText("Pick variable from Dataset 2 that corresponds to Variable 3 in Dataset 1.  Necessary if column names do not match.");

    by23ComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        by23ComboBoxActionPerformed(evt);
      }
    });

    byall2CheckBox.setSelected(true);
    byall2CheckBox.setText("By All");
    byall2CheckBox.setToolTipText("If selected, then all rows in Dataset 2 will be included, even if there is no corresponding row in Dataset 1.");
    byall2CheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));

    suffix2Label.setText("Suffix:");
    suffix2Label.setToolTipText("If column names match in Dataset 1 and 2, then this suffix will be added to the column name for Dataset 2.");

    suffix2.setText(".2");

    javax.swing.GroupLayout dataset2PaneLayout = new javax.swing.GroupLayout(dataset2Pane);
    dataset2Pane.setLayout(dataset2PaneLayout);
    dataset2PaneLayout.setHorizontalGroup(
      dataset2PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(dataset2PaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(dataset2PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addGroup(dataset2PaneLayout.createSequentialGroup()
            .addComponent(dataset2Label)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(dataset2ComboBox, 0, 230, Short.MAX_VALUE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(browse2Button)
            .addContainerGap())
          .addGroup(dataset2PaneLayout.createSequentialGroup()
            .addGap(12, 12, 12)
            .addGroup(dataset2PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
              .addComponent(by22Label)
              .addComponent(by21Label)
              .addComponent(by23Label)
              .addComponent(suffix2Label))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(dataset2PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
              .addComponent(suffix2, javax.swing.GroupLayout.PREFERRED_SIZE, 80, javax.swing.GroupLayout.PREFERRED_SIZE)
              .addGroup(dataset2PaneLayout.createSequentialGroup()
                .addGroup(dataset2PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                  .addComponent(by22ComboBox, 0, 325, Short.MAX_VALUE)
                  .addComponent(by21ComboBox, 0, 325, Short.MAX_VALUE)
                  .addComponent(by23ComboBox, 0, 325, Short.MAX_VALUE)
                  .addComponent(byall2CheckBox))
                .addContainerGap())))))
    );
    dataset2PaneLayout.setVerticalGroup(
      dataset2PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, dataset2PaneLayout.createSequentialGroup()
        .addGroup(dataset2PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(dataset2Label)
          .addComponent(dataset2ComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(browse2Button))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(dataset2PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(by21Label)
          .addComponent(by21ComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(dataset2PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(by22Label)
          .addComponent(by22ComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(dataset2PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(by23Label)
          .addComponent(by23ComboBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(byall2CheckBox)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(dataset2PaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(suffix2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(suffix2Label))
        .addContainerGap(20, Short.MAX_VALUE))
    );

    datasetPane.add(dataset2Pane);

    savePane.setBorder(javax.swing.BorderFactory.createTitledBorder("Save Options"));
    savePane.setLayout(new java.awt.GridLayout(1, 0));

    sortCheck.setText("Sort Output Data");
    sortCheck.setToolTipText("If selected, merged data will be sorted based on the columns used for merging.  If not selected, rows remain in order of dataset 1, with non-intersecting rows coming at the end, dataset 1 before dataset 2.");
    sortCheck.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));

    rSaveLabel.setText("Local Name:");
    rSaveLabel.setToolTipText("This sets the variable name in R (and thus how the name will appear in the dataset lists).");

    rNameText.setText("mergedData");

    javax.swing.GroupLayout saveLeftPaneLayout = new javax.swing.GroupLayout(saveLeftPane);
    saveLeftPane.setLayout(saveLeftPaneLayout);
    saveLeftPaneLayout.setHorizontalGroup(
      saveLeftPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(saveLeftPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(saveLeftPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(sortCheck)
          .addGroup(saveLeftPaneLayout.createSequentialGroup()
            .addComponent(rSaveLabel)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(rNameText, javax.swing.GroupLayout.DEFAULT_SIZE, 332, Short.MAX_VALUE)))
        .addContainerGap())
    );
    saveLeftPaneLayout.setVerticalGroup(
      saveLeftPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(saveLeftPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(sortCheck)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(saveLeftPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(rSaveLabel)
          .addComponent(rNameText, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addContainerGap(14, Short.MAX_VALUE))
    );

    savePane.add(saveLeftPane);

    saveCheck.setSelected(true);
    saveCheck.setText("Save to File");
    saveCheck.setToolTipText("If checked, the merged dataset will be written out as a tab-delimited text file to the location specified below.");
    saveCheck.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
    saveCheck.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        saveCheckActionPerformed(evt);
      }
    });

    saveLabel.setText("Filename:");
    saveLabel.setToolTipText("Name (and path) under which to save the merged file.  A .txt extension is recommended, since the output will be tab-delimited text.");

    nameText.setText("mergedData.txt");

    outputBrowse.setText("Browse...");
    outputBrowse.setMargin(new java.awt.Insets(0, 5, 0, 5));
    outputBrowse.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        outputBrowseActionPerformed(evt);
      }
    });

    javax.swing.GroupLayout saveRightPaneLayout = new javax.swing.GroupLayout(saveRightPane);
    saveRightPane.setLayout(saveRightPaneLayout);
    saveRightPaneLayout.setHorizontalGroup(
      saveRightPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(saveRightPaneLayout.createSequentialGroup()
        .addGroup(saveRightPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addGroup(saveRightPaneLayout.createSequentialGroup()
            .addContainerGap()
            .addComponent(saveCheck))
          .addGroup(saveRightPaneLayout.createSequentialGroup()
            .addGap(55, 55, 55)
            .addComponent(saveLabel)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(nameText, javax.swing.GroupLayout.PREFERRED_SIZE, 200, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(outputBrowse)))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    saveRightPaneLayout.setVerticalGroup(
      saveRightPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(saveRightPaneLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(saveCheck)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(saveRightPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(saveLabel)
          .addComponent(nameText, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(outputBrowse))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    savePane.add(saveRightPane);

    buttonPane.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 10, 5));

    cancelButton.setText("Cancel");
    cancelButton.setMargin(new java.awt.Insets(0, 10, 0, 10));
    cancelButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        cancelButtonActionPerformed(evt);
      }
    });
    buttonPane.add(cancelButton);

    submitButton.setText("Submit");
    submitButton.setMargin(new java.awt.Insets(0, 10, 0, 10));
    submitButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        submitButtonActionPerformed(evt);
      }
    });
    buttonPane.add(submitButton);

    helpButton.setText("Help");
    helpButton.setMargin(new java.awt.Insets(0, 10, 0, 10));
    helpButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        helpButtonActionPerformed(evt);
      }
    });
    buttonPane.add(helpButton);

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
    getContentPane().setLayout(layout);
    layout.setHorizontalGroup(
      layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addComponent(datasetPane, javax.swing.GroupLayout.DEFAULT_SIZE, 966, Short.MAX_VALUE)
      .addComponent(savePane, javax.swing.GroupLayout.DEFAULT_SIZE, 966, Short.MAX_VALUE)
      .addComponent(buttonPane, javax.swing.GroupLayout.DEFAULT_SIZE, 966, Short.MAX_VALUE)
    );
    layout.setVerticalGroup(
      layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(layout.createSequentialGroup()
        .addComponent(datasetPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(savePane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(buttonPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
  }// </editor-fold>//GEN-END:initComponents

  private void saveCheckActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_saveCheckActionPerformed
  {//GEN-HEADEREND:event_saveCheckActionPerformed
    saveLabel.setEnabled(saveCheck.isSelected());
    nameText.setEnabled(saveCheck.isSelected());
    outputBrowse.setEnabled(saveCheck.isSelected());
  }//GEN-LAST:event_saveCheckActionPerformed

  private void outputBrowseActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_outputBrowseActionPerformed
  {//GEN-HEADEREND:event_outputBrowseActionPerformed
    setTextUsingFileChooser(nameText, JFileChooser.FILES_ONLY);
  }//GEN-LAST:event_outputBrowseActionPerformed

  private void by23ComboBoxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_by23ComboBoxActionPerformed
  {//GEN-HEADEREND:event_by23ComboBoxActionPerformed
    refreshValidity();
  }//GEN-LAST:event_by23ComboBoxActionPerformed

  private void by22ComboBoxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_by22ComboBoxActionPerformed
  {//GEN-HEADEREND:event_by22ComboBoxActionPerformed
    refreshValidity();
  }//GEN-LAST:event_by22ComboBoxActionPerformed

  private void by21ComboBoxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_by21ComboBoxActionPerformed
  {//GEN-HEADEREND:event_by21ComboBoxActionPerformed
    refreshValidity();
  }//GEN-LAST:event_by21ComboBoxActionPerformed

  private void by13ComboBoxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_by13ComboBoxActionPerformed
  {//GEN-HEADEREND:event_by13ComboBoxActionPerformed
    refreshValidity();
  }//GEN-LAST:event_by13ComboBoxActionPerformed

  private void by12ComboBoxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_by12ComboBoxActionPerformed
  {//GEN-HEADEREND:event_by12ComboBoxActionPerformed
    refreshValidity();
  }//GEN-LAST:event_by12ComboBoxActionPerformed

  private void by11ComboBoxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_by11ComboBoxActionPerformed
  {//GEN-HEADEREND:event_by11ComboBoxActionPerformed
    refreshValidity();
  }//GEN-LAST:event_by11ComboBoxActionPerformed

  private void dataset2ComboBoxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_dataset2ComboBoxActionPerformed
  {//GEN-HEADEREND:event_dataset2ComboBoxActionPerformed
    this.refreshVariableLists(dataset2ComboBox);
  }//GEN-LAST:event_dataset2ComboBoxActionPerformed

  private void browse2ButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_browse2ButtonActionPerformed
  {//GEN-HEADEREND:event_browse2ButtonActionPerformed
    DataLoader loader = new DataLoader();
    String dataName = loader.getDataName();

    JGR.MAINRCONSOLE.toBack();

    if (dataName != null && dataName.length() > 0)
    {
    }
  }//GEN-LAST:event_browse2ButtonActionPerformed

    private void browse1ButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_browse1ButtonActionPerformed
    {//GEN-HEADEREND:event_browse1ButtonActionPerformed
      DataLoader loader = new DataLoader();
      String dataName = loader.getDataName();

      JGR.MAINRCONSOLE.toBack();

      if (dataName != null && dataName.length() > 0)
      {
      }
    }//GEN-LAST:event_browse1ButtonActionPerformed

  private void dataset1ComboBoxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_dataset1ComboBoxActionPerformed
  {//GEN-HEADEREND:event_dataset1ComboBoxActionPerformed
    this.refreshVariableLists(dataset1ComboBox);
  }//GEN-LAST:event_dataset1ComboBoxActionPerformed

private void submitButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_submitButtonActionPerformed
    String cmd = "datamerge.JGR("
    + "x=" + (this.getDataset1ComboBox() != null && this.getDataset1ComboBox().getSelectedIndex() != -1 ? this.getDataset1ComboBox().getSelectedItem() : "NULL")
    + ", y=" + (this.getDataset2ComboBox() != null && this.getDataset2ComboBox().getSelectedIndex() != -1 ? this.getDataset2ComboBox().getSelectedItem() : "NULL")
    + ", by11=" + GUIUtils.getSelectedItemR(by11ComboBox)
    + ", by12=" + GUIUtils.getSelectedItemR(by12ComboBox)
    + ", by13=" + GUIUtils.getSelectedItemR(by13ComboBox)
    + ", by21=" + GUIUtils.getSelectedItemR(by21ComboBox)
    + ", by22=" + GUIUtils.getSelectedItemR(by22ComboBox)
    + ", by23=" + GUIUtils.getSelectedItemR(by23ComboBox)
    + ", all.x=" + GUIUtils.getBooleanValueR(byall1CheckBox)
    + ", all.y=" + GUIUtils.getBooleanValueR(byall2CheckBox)
    + ", suffixes="+ "c(" + RUtils.getStringValue(suffix1.getText()) + "," + RUtils.getStringValue(suffix2.getText()) + ")"
    + ", rName=" + RUtils.getStringValue(rNameText.getText())
    + ", sort=" + GUIUtils.getBooleanValueR(sortCheck)
    + ", writeout=" + GUIUtils.getBooleanValueR(saveCheck)
    + ", outName=" + RUtils.getStringValue(nameText.getText())
    + ")";
    JGR.MAINRCONSOLE.execute(cmd, true);
}//GEN-LAST:event_submitButtonActionPerformed

private void helpButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_helpButtonActionPerformed
  String cmd = "CADStat.help('loaddata.JGR')";
  JGR.MAINRCONSOLE.execute(cmd, true);
}//GEN-LAST:event_helpButtonActionPerformed

private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
  this.dispose();
}//GEN-LAST:event_cancelButtonActionPerformed
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton browse1Button;
  private javax.swing.JButton browse2Button;
  private javax.swing.JPanel buttonPane;
  private javax.swing.JComboBox by11ComboBox;
  private javax.swing.JLabel by11Label;
  private javax.swing.JComboBox by12ComboBox;
  private javax.swing.JLabel by12Label;
  private javax.swing.JComboBox by13ComboBox;
  private javax.swing.JLabel by13Label;
  private javax.swing.JComboBox by21ComboBox;
  private javax.swing.JLabel by21Label;
  private javax.swing.JComboBox by22ComboBox;
  private javax.swing.JLabel by22Label;
  private javax.swing.JComboBox by23ComboBox;
  private javax.swing.JLabel by23Label;
  private javax.swing.JCheckBox byall1CheckBox;
  private javax.swing.JCheckBox byall2CheckBox;
  private javax.swing.JButton cancelButton;
  private javax.swing.JComboBox dataset1ComboBox;
  private javax.swing.JLabel dataset1Label;
  private javax.swing.JPanel dataset1Pane;
  private javax.swing.JComboBox dataset2ComboBox;
  private javax.swing.JLabel dataset2Label;
  private javax.swing.JPanel dataset2Pane;
  private javax.swing.JPanel datasetPane;
  private javax.swing.JButton helpButton;
  private javax.swing.JTextField nameText;
  private javax.swing.JButton outputBrowse;
  private javax.swing.JTextField rNameText;
  private javax.swing.JLabel rSaveLabel;
  private javax.swing.JCheckBox saveCheck;
  private javax.swing.JLabel saveLabel;
  private javax.swing.JPanel saveLeftPane;
  private javax.swing.JPanel savePane;
  private javax.swing.JPanel saveRightPane;
  private javax.swing.JCheckBox sortCheck;
  private javax.swing.JButton submitButton;
  private javax.swing.JTextField suffix1;
  private javax.swing.JLabel suffix1Label;
  private javax.swing.JTextField suffix2;
  private javax.swing.JLabel suffix2Label;
  // End of variables declaration//GEN-END:variables

  public void valueChanged(javax.swing.event.ListSelectionEvent listSelectionEvent)
  {
    refreshValidity();
  }

  @Override
  public void windowOpened(WindowEvent e)
  {
  }

  @Override
  public void windowClosing(WindowEvent e)
  {
  }

  @Override
  public void windowClosed(WindowEvent e)
  {
  }

  @Override
  public void windowIconified(WindowEvent e)
  {
  }

  @Override
  public void windowDeiconified(WindowEvent e)
  {
  }

  @Override
  public void windowActivated(WindowEvent e)
  {
    this.refreshDatasetLists();
  }

  @Override
  public void windowDeactivated(WindowEvent e)
  {
  }

  /**
   * @return the datasetVariableMap
   */
  public Map<JComboBox, List<JComboBox>> getDatasetVariableMap()
  {
    return datasetVariableMap;
  }

  /**
   * @param datasetVariableMap the datasetVariableMap to set
   */
  public void setDatasetVariableMap(Map<JComboBox, List<JComboBox>> datasetVariableMap)
  {
    this.datasetVariableMap = datasetVariableMap;
  }
}

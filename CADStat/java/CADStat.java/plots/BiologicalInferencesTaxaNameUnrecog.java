/*
 * BiologicalInferencesTaxaNameUnrecog.java
 *
 * Created on November 13, 2008, 11:10 AM
 */
package org.neptuneinc.cadstat.plots;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.GroupLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.SpringLayout;
import javax.swing.WindowConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import org.rosuda.JGR.JGR;

/**
 *
 * @author  Liejun.Wu
 * We use this java class to get the unrecognized taxa name from R; display them to user;
 * user will input the correct taxa name; finally pass the user input to a R function. 
 */
public class BiologicalInferencesTaxaNameUnrecog extends JFrame implements ListSelectionListener, DocumentListener
{
  /** constructor */
  public BiologicalInferencesTaxaNameUnrecog()
  {
    //initComponents();
  }

  /** constructor with param; num = how many of unrecognized taxon names
  str = one taxa names
   */
  public BiologicalInferencesTaxaNameUnrecog(int num, String str)
  {
    numOfTaxa = num;
    taxaName = new String[numOfTaxa];
    taxaName[0] = str;
    initComponents();
  }

  /** constructor with param; num = how many of unrecognized taxon names
  str = array of taxa names
   */
  public BiologicalInferencesTaxaNameUnrecog(int num, String str[])
  {
    numOfTaxa = num;
    taxaName = new String[numOfTaxa];
    taxaName = str;
    initComponents();
  }

  /** This method is called from within the constructor to
   * initialize the form.
   */
  private void initComponents()
  {

    //create object references of JLabel and JTextField
    taxaNameOrigLabel = new JLabel[numOfTaxa];
    taxaNameCorrTextField = new JTextField[numOfTaxa];
    //allocate memory of each of object - JLable and JTextField
    for (int i = 0; i < numOfTaxa; i++)
    {
      taxaNameOrigLabel[i] = new JLabel();
      //set TextField length to 20
      taxaNameCorrTextField[i] = new JTextField(20);
    }
    for (int i = 0; i < numOfTaxa; i++)
    {
      taxaNameOrigLabel[i].setText(taxaName[i]);
      taxaNameCorrTextField[i].setText(taxaName[i]);
    }

    title = new JLabel();
    okButton = new JButton();
    cancelButton = new JButton();
    taxaNamePanel = new JPanel();
    taxaNameScrollPane = new JScrollPane();

    title.setFont(new java.awt.Font("Tahoma", 0, 13));
    title.setText("Please correct the following unrecognized taxon names (changes will not be saved in original data):");

    okButton.setText("OK");
    okButton.addActionListener(new ActionListener()
    {
      @Override
      public void actionPerformed(ActionEvent evt)
      {
        okButtonActionPerformed(evt);
      }
    });

    cancelButton.setText("Cancel");
    cancelButton.addActionListener(new ActionListener()
    {
      @Override
      public void actionPerformed(ActionEvent evt)
      {
        cancelButtonActionPerformed(evt);
      }
    });

    setLocation(200, 200);
    setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    setTitle("Biological Inferences");

    //put the JLabel and JTextField in a JPanel and set springLayout on the panel
    SpringLayout taxaNamePanelLayout = new SpringLayout();
    taxaNamePanel.setLayout(taxaNamePanelLayout);
    for (int i = 0; i < numOfTaxa; i++)
    {
      taxaNamePanel.add(taxaNameOrigLabel[i]);
      taxaNamePanel.add(taxaNameCorrTextField[i]);
    }
    SpringUtilities.makeCompactGrid(taxaNamePanel,
      numOfTaxa, 2, //rows, cols
      6, 6, //initX, initY
      6, 6); //xPad, yPad

    /*If set the height of taxaNameCorrTextField to 25, then taxaNameScrollPane with
     * height 300 will hold up to 12(300/25=12) textField. If numOfTaxaName<12, we have to set
     * the scrollPane's hight to a smaller number, so we keep the hight of each textField as 25.
     */
    int preferredHigh;
    if (numOfTaxa < 12)
    {
      preferredHigh = numOfTaxa * 25 + 30; //+30:need some space for gap
    }
    else
    {
      preferredHigh = 300;
    }

    taxaNameScrollPane = new JScrollPane(taxaNamePanel);
    taxaNameScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
    taxaNameScrollPane.setPreferredSize(new Dimension(400, preferredHigh));
    taxaNameScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

    //GroupLayout on JFrame, includes the title, panel, and button
    GroupLayout layout = new GroupLayout(getContentPane());
    getContentPane().setLayout(layout);
    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.Alignment.LEADING).addGroup(layout.createSequentialGroup().addGap(74, 74, 74).addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING).addComponent(taxaNameScrollPane, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE).addGroup(layout.createSequentialGroup().addComponent(okButton).addGap(52, 52, 52).addComponent(cancelButton)).addComponent(title)).addGap(80, 80, 80)));
    layout.setVerticalGroup(
      layout.createParallelGroup(GroupLayout.Alignment.LEADING).addGroup(layout.createSequentialGroup().addGap(25, 25, 25).addComponent(title).addGap(18, 18, 18).addComponent(taxaNameScrollPane, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE).addGap(18, 18, 18).addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE).addComponent(okButton).addComponent(cancelButton)).addContainerGap(71, Short.MAX_VALUE)));

    pack();
  }// </editor-fold>

  private JFrame getMyGUI(int num, String str)
  {
    JFrame jf = new BiologicalInferencesTaxaNameUnrecog(num, str);
    jf.setVisible(true);
    return jf;
  }

  //Method will be called by R to initial the Java interface
  private JFrame getMyGUI(int num, String str[])
  {

    JFrame jf = new BiologicalInferencesTaxaNameUnrecog(num, str);
    jf.setVisible(true);
    return jf;
  }

  //User click OK, Java will call the R function and pass the user input taxa name(as vector data type).
  private void okButtonActionPerformed(ActionEvent evt)
  {
    //Validate the JTextField, if any field is empty, user will get a popup box.
    int j = 0;
    for (int i = 0; i < numOfTaxa; i++)
    {
      if (taxaNameCorrTextField[i].getText().length() == 0)
      {
        j = j + 1;
      }
    }
    if (j > 0)
    {
      System.out.println("No empty taxa name allowed");
      JOptionPane.showMessageDialog(null, "Empty taxa name aren't allowed! Unrecognized taxa will be ignored");
    }
    else
    {
      System.out.println("OK button is pushed");

      String str = "";
      for (int i = 0; i < numOfTaxa; i++)
      {
        if (i == numOfTaxa - 1)
        {
          str = str + "\"" + taxaNameCorrTextField[i].getText() + "\"";
        }
        else
        {
          str = str + "\"" + taxaNameCorrTextField[i].getText() + "\"" + ",";
        }
      }

      String cmd ="bioinfer.JGR(tname.new=c(" + str + "))";
      JGR.MAINRCONSOLE.execute(cmd, true);
      this.dispose();
    }

  }

  private void cancelButtonActionPerformed(ActionEvent evt)
  {
    this.dispose();
  }
  // Variables declaration - do not modify
  private JLabel title;
  private JButton okButton;
  private JButton cancelButton;
  private JPanel taxaNamePanel;
  private JScrollPane taxaNameScrollPane;
  private JLabel taxaNameOrigLabel[];
  private JTextField taxaNameCorrTextField[];
  private int numOfTaxa;
  private String taxaName[];

  @Override
  public void valueChanged(ListSelectionEvent e)
  {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public void insertUpdate(DocumentEvent e)
  {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public void removeUpdate(DocumentEvent e)
  {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public void changedUpdate(DocumentEvent e)
  {
    throw new UnsupportedOperationException("Not supported yet.");
  }
}

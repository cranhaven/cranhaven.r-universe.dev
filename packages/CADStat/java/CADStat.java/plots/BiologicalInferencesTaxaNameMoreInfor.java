/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.neptuneinc.cadstat.plots;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.GroupLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.LayoutStyle;
import javax.swing.WindowConstants;
import javax.swing.table.DefaultTableModel;
import org.rosuda.JGR.JGR;

/**
 *
 * @author liejun.wu
 * We use this java class to display the static content of the unrecognized taxa.
 * Later, we decided to display the content in JGR window. I still keep this class,
 * in case we need to use in the future.
 */
public class BiologicalInferencesTaxaNameMoreInfor extends JFrame
{
  public BiologicalInferencesTaxaNameMoreInfor()
  {
  }

  public BiologicalInferencesTaxaNameMoreInfor(int num, String str[], int occurance[])
  {
    numOfTaxa = num;
    taxaName = str;
    numOfOccurrences = occurance;
    initComponents();
  }

  private void initComponents()
  {
    title = new JLabel();
    okButton = new JButton();
    taxaNameScrollPane = new JScrollPane();
    taxaNameTable = new JTable();

    setLocation(100, 100);
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    setTitle("Biological Inferences");

    title.setFont(new java.awt.Font("Tahoma", 0, 13));
    title.setText("Summary of taxa without matches:");

    okButton.setText("OK");
    okButton.addActionListener(new ActionListener()
    {
      @Override
      public void actionPerformed(ActionEvent evt)
      {
        jButton1ActionPerformed(evt);
      }
    });

    columnNames = new String[]
      {
        "index",
        "Taxa Name",
        "Number of occurrences"
      };
    CreateRowDate();

    taxaNameTable.setModel(new DefaultTableModel(
      rowData, columnNames)
    {
      Class[] types = new Class[]
      {
        java.lang.Integer.class,
        java.lang.String.class,
        java.lang.Integer.class
      };

      @Override
      public Class getColumnClass(int columnIndex)
      {
        return types[columnIndex];
      }

      ;
      boolean[] canEdit = new boolean[]
      {
        false,
        false,
        false,
        false
      };

      @Override
      public boolean isCellEditable(int rowIndex, int columnIndex)
      {
        return canEdit[columnIndex];
      }
    });
    taxaNameTable.getColumnModel().getColumn(0).setPreferredWidth(5);
    taxaNameScrollPane.setViewportView(taxaNameTable);

    int preferredHigh;// hight for taxaNameScrollPane
    if (numOfTaxa < 15)
    {
      preferredHigh = numOfTaxa * 20; //preferHight<300;20 per recored;300/20=15
    }
    else
    {
      preferredHigh = 300;
    }

    GroupLayout layout = new GroupLayout(getContentPane());
    getContentPane().setLayout(layout);
    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.Alignment.LEADING).addGroup(layout.createSequentialGroup().addGap(80, 80, 80).addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING).addGroup(layout.createSequentialGroup().addGap(30, 30, 30).addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING).addComponent(taxaNameScrollPane, GroupLayout.PREFERRED_SIZE, 400, GroupLayout.PREFERRED_SIZE).addComponent(title))).addGroup(layout.createSequentialGroup().addGap(200, 200, 200).addComponent(okButton))).addContainerGap(120, Short.MAX_VALUE)));
    layout.setVerticalGroup(
      layout.createParallelGroup(GroupLayout.Alignment.LEADING).addGroup(layout.createSequentialGroup().addContainerGap(20, Short.MAX_VALUE).addComponent(title).addGap(40, 40, 40).addComponent(taxaNameScrollPane, GroupLayout.PREFERRED_SIZE, preferredHigh, GroupLayout.PREFERRED_SIZE).addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, 50, Short.MAX_VALUE).addComponent(okButton).addGap(38, 38, 38)));

    pack();
  }

  private void jButton1ActionPerformed(ActionEvent evt)
  {
    String cmd = "okPushed<<-\"true\"";
    //call R function and pass true value to variable "okPushed"
    JGR.MAINRCONSOLE.execute(cmd, true);
    this.dispose();
  }

  public void CreateRowDate()
  {
    rowData = new Object[numOfTaxa][3];
    for (int i = 0; i < 3; i++)
    {
      for (int j = 0; j < numOfTaxa; j++)
      {
        if (i == 0)
        {
          rowData[j][i] = j + 1;
        }
        else if (i == 1)
        {
          rowData[j][i] = taxaName[j];
        }
        else
        {
          rowData[j][i] = numOfOccurrences[j];
        }
      }
    }
  }

  private JFrame getMyGUI(int num, String str[], int occurance[])
  {
    JFrame jf = new BiologicalInferencesTaxaNameMoreInfor(num, str, occurance);
    jf.setVisible(true);
    return jf;
  }

  public static void main(String args[])
  {
    java.awt.EventQueue.invokeLater(new Runnable()
    {
      @Override
      public void run()
      {
        int num = 5;
        String str[] =
        {
          "liejun11111",
          "liejun22222",
          "liejun3333",
          "liejun3333",
          "liejun4444"
        };
        int occurrencesNum[] =
        {
          1,
          2,
          3,
          4,
          5
        };
        new BiologicalInferencesTaxaNameMoreInfor(num, str, occurrencesNum).setVisible(true);
      }
    });
  }
  private JButton okButton;
  private JLabel title;
  private JScrollPane taxaNameScrollPane;
  private JTable taxaNameTable;
  private String columnNames[];
  private Object rowData[][];
  private int numOfTaxa;
  private String taxaName[];
  private int numOfOccurrences[];
}

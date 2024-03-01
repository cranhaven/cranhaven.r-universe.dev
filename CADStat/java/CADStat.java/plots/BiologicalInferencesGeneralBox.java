/*
 * BiologicalInferencesGeneralBox.java
 *
 * Created on November 21, 2008, 5:10 PM
 */
package org.neptuneinc.cadstat.plots;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.GroupLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.WindowConstants;

/**
 *
 * @author  Liejun.Wu
 * We use this java class as the general popup box; R will call the
 * java class and pass the message to the popup box.
 */
public class BiologicalInferencesGeneralBox extends JFrame
{
  public BiologicalInferencesGeneralBox()
  {
  }

  public BiologicalInferencesGeneralBox(String titleStr, String labelStr)
  {
    initComponents(titleStr, labelStr);
  }

  private void initComponents(String titleStr, String labelStr)
  {
    label = new JLabel();
    okButton = new JButton();

    label.setFont(new java.awt.Font("Tahoma", 0, 13));
    label.setText(labelStr);

    okButton.setText("OK");
    okButton.addActionListener(new ActionListener()
    {
      @Override
      public void actionPerformed(ActionEvent evt)
      {
        okButtonActionPerformed(evt);
      }
    });

    setLocation(300, 300);
    setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    setTitle(titleStr);

    GroupLayout layout = new GroupLayout(getContentPane());
    getContentPane().setLayout(layout);
    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.Alignment.LEADING).addGroup(layout.createSequentialGroup().addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING).addGroup(layout.createSequentialGroup().addGap(74, 74, 74).addComponent(label)).addGroup(layout.createSequentialGroup().addGap(120, 120, 120).addComponent(okButton, GroupLayout.PREFERRED_SIZE, 65, GroupLayout.PREFERRED_SIZE))).addContainerGap(100, Short.MAX_VALUE)));
    layout.setVerticalGroup(
      layout.createParallelGroup(GroupLayout.Alignment.LEADING).addGroup(layout.createSequentialGroup().addGap(25, 25, 25).addComponent(label).addGap(43, 43, 43).addComponent(okButton).addContainerGap(40, Short.MAX_VALUE)));

    pack();
  }

  public JFrame getMyGUI(String titleStr, String labelStr)
  {
    JFrame jf = new BiologicalInferencesGeneralBox(titleStr, labelStr);
    jf.setVisible(true);
    return jf;
  }

  private void okButtonActionPerformed(ActionEvent evt)
  {
    this.dispose();
  }
  private JButton okButton;
  private JLabel label;
}

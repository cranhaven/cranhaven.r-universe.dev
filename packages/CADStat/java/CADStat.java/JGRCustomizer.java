/*
 * To change this template,  choose Tools | Templates
 * and open the template in the editor.
 */
package org.neptuneinc.cadstat;

import javax.swing.JMenu;
import javax.swing.JMenuBar;
import org.rosuda.JGR.JGR;

/**
 *
 * @author Pasha Minallah
 */
public class JGRCustomizer
{
  public void removeMenu(String name)
  {
    JMenuBar menuBar = JGR.MAINRCONSOLE.getJMenuBar();

    if (menuBar != null)
    {
      for (int i = menuBar.getMenuCount(), n = 0; i > n; --i)
      {
        JMenu menu = menuBar.getMenu(i);

        if (menu != null && (menu.getText().compareTo(name) == 0))
        {
          menuBar.remove(menu);
        }
      }
    }
  }

  public void customize()
  { 
    customizeMenus();
  }

  public void customizeMenus()
  {
    JMenuBar menuBar = JGR.MAINRCONSOLE.getJMenuBar();

    if (menuBar != null)
    {
      for (int i = menuBar.getMenuCount(), n = 0; i > n; --i)
      {
        JMenu menu = menuBar.getMenu(i);

        if (menu != null && !(menu.getText().compareTo("Workspace") == 0 || menu.getText().compareTo("Tools") == 0 || menu.getText().compareTo("File") == 0 || menu.getText().compareTo("Edit") == 0))
        {
          menuBar.remove(menu);
        }
      }
    }
  }
}

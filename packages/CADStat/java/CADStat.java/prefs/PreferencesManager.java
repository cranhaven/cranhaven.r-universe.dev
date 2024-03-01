/*
 * PreferencesManager.java
 *
 * Created on October 17, 2005, 9:24 AM
 */
package org.neptuneinc.cadstat.prefs;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.prefs.BackingStoreException;
import java.util.prefs.InvalidPreferencesFormatException;
import java.util.prefs.Preferences;

/**
 *
 * @author Pasha Minallah
 */
public class PreferencesManager
{
  /** Preferences File */
  private File prefsFile;
  /** Preferences for current package */
  private Preferences prefs;
  /** CADStat Preferences */
  private CadstatPreferences cadstatPrefs;

  /** Creates a new instance of PreferencesManager */
  public PreferencesManager()
  {
    this.setPreferences(Preferences.userNodeForPackage(org.neptuneinc.cadstat.prefs.CadstatPreferences.class));
    this.setCadstatPreferences(new CadstatPreferences());

    this.setPreferencesFile(new File(System.getProperty("user.home") + File.separator + ".cadstat", "preferences.xml"));
  }

  public PreferencesManager(File prefsFile)
  {
    this.setPreferences(Preferences.userNodeForPackage(org.neptuneinc.cadstat.prefs.CadstatPreferences.class));
    this.setCadstatPreferences(new CadstatPreferences());

    this.setPreferencesFile(prefsFile);
  }

  public void importPreferences() throws FileNotFoundException, IOException, InvalidPreferencesFormatException
  {
      try{
    Preferences.importPreferences(new FileInputStream(this.getPreferencesFile()));
      } catch (FileNotFoundException e){
          System.out.println("Preferences file not found at '" + this.getPreferencesFile().getAbsolutePath() + "'. Using default preferences.");
      }
    this.getCadstatPreferences().setDataPointColor(DataPointColor.valueOf(this.getPreferences().get("DataPointColor", DataPointColor.BLACK.name())));
    this.getCadstatPreferences().setPlotCharacter(PlotCharacter.valueOf(this.getPreferences().get("PlotChar", PlotCharacter.OPEN_CIRCLE.name())));
    this.getCadstatPreferences().setPlotTitleMagnification(this.getPreferences().getFloat("PlotTitleMag", CadstatPreferences.DEFAULT_MAGNIFICATION));
    this.getCadstatPreferences().setAxisMagnification(this.getPreferences().getFloat("AxisMag", CadstatPreferences.DEFAULT_MAGNIFICATION));
    this.getCadstatPreferences().setAxisLabelMagnification(this.getPreferences().getFloat("AxisLabelMag", CadstatPreferences.DEFAULT_MAGNIFICATION));
    this.getCadstatPreferences().setDataPointMagnification(this.getPreferences().getFloat("DataPointMag", CadstatPreferences.DEFAULT_MAGNIFICATION));
  }

  public void exportPreferences() throws FileNotFoundException, IOException, BackingStoreException
  {
    if (this.getPreferencesFile() != null && !this.getPreferencesFile().exists())
    {
      this.getPreferencesFile().getParentFile().mkdirs();
      this.getPreferencesFile().createNewFile();
    }

    this.getPreferences().put("DataPointColor", this.getCadstatPreferences().getDataPointColor().name());
    this.getPreferences().put("PlotChar", this.getCadstatPreferences().getPlotCharacter().name());
    this.getPreferences().putFloat("PlotTitleMag", this.getCadstatPreferences().getPlotTitleMagnification());
    this.getPreferences().putFloat("AxisMag", this.getCadstatPreferences().getAxisMagnification());
    this.getPreferences().putFloat("AxisLabelMag", this.getCadstatPreferences().getAxisLabelMagnification());
    this.getPreferences().putFloat("DataPointMag", this.getCadstatPreferences().getDataPointMagnification());

    this.getPreferences().exportSubtree(new FileOutputStream(this.getPreferencesFile()));
  }

  public Preferences getPreferences()
  {
    return prefs;
  }

  public void setPreferences(Preferences prefs)
  {
    this.prefs = prefs;
  }

  public File getPreferencesFile()
  {
    return prefsFile;
  }

  public void setPreferencesFile(File prefsFile)
  {
    this.prefsFile = prefsFile;
  }

  public CadstatPreferences getCadstatPreferences()
  {
    return cadstatPrefs;
  }

  public void setCadstatPreferences(CadstatPreferences cadstatPrefs)
  {
    this.cadstatPrefs = cadstatPrefs;
  }
}

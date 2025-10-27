using FileTools.Infrastructure;
using Bundle.Misc;
using Bundle.SideFrame.Derived.Children;
using Excel;
using FileTools.Base;
using Microsoft.Office.Interop.Excel;
using SplashScreen;
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Windows.Forms;
using static Excel.Header_DataManager;
using static Excel.Prego;
using static Excel.StaticHelpers;
using static FileTools.CommonData.CommonData;
using static FileTools.Properties.Settings;
using static FileTools.StaticFileTools;
using static System.Windows.Forms.VisualStyles.VisualStyleElement;
using CheckBox = System.Windows.Forms.CheckBox;
using TextBox = System.Windows.Forms.TextBox;

namespace Bundle
{
    public partial class BundleUI : Form
    {
        private ComObjectManager _comManager;

        public BundleUI()
        {
            InitializeComponent();
            _comManager = new ComObjectManager();
        }
        #region Events

        private void BundleUI_Load(object sender, EventArgs e)
      {
          try
            {
   GlobalErrorHandler.LogInfo("BundleUI_Load started");

  // Prego imports
           tBundleWidth.Text = Bundle_Width.ToString();
        cSideFrameTHK.Text = SideFrame_THK.ToString();
          tDepth.Text = SideFrame_Depth.ToString();
      cHeadersOutsideFrame.Checked = HeadersOutsideFrames;
       tTubeLength.Text = TubeLength.ToString();
      tTubeProjection.Text = TubeProjection.ToString();
        tFrontFinStripBack.Text = FinStripBack_Front.ToString();
     tRearFinStripBack.Text = FinStripBack_Rear.ToString();
           tTubeOD.Text = TubeOD.ToString();
tTubeWallTHK.Text = TubeWallTHK.ToString();
            tFinOD.Text = FinOD.ToString();
     tTubes_Row_1L.Text = Tube_Row_1L.ToString();
                tTubes_Row_2L.Text = Tube_Row_2L.ToString();
    tTubeHorizPitch.Text = TubeHorizPitch.ToString();
       tTubeQuantity.Text = TubeQuantity.ToString();
              tFrontVerticalPitch_1_2.Text = FrontVerticalPitch._1_2.ToString();
         tFrontVerticalPitch_2_3.Text = FrontVerticalPitch._2_3.ToString();
     tFrontVerticalPitch_3_4.Text = FrontVerticalPitch._3_4.ToString();
  tFrontVerticalPitch_4_5.Text = FrontVerticalPitch._4_5.ToString();
     tFrontVerticalPitch_5_6.Text = FrontVerticalPitch._5_6.ToString();
          tFrontVerticalPitch_6_7.Text = FrontVerticalPitch._6_7.ToString();
     tFrontVerticalPitch_7_8.Text = FrontVerticalPitch._7_8.ToString();
      tFrontVerticalPitch_8_9.Text = FrontVerticalPitch._8_9.ToString();
     tFrontVerticalPitch_9_10.Text = FrontVerticalPitch._9_10.ToString();
    tTubeSupportSpacing_Feet.Text = TubeSupportSpacing_Feet.ToString();
       tRearVerticalPitch_1_2.Text = RearVerticalPitch._1_2.ToString();
   tRearVerticalPitch_2_3.Text = RearVerticalPitch._2_3.ToString();
        tRearVerticalPitch_3_4.Text = RearVerticalPitch._3_4.ToString();
             tRearVerticalPitch_4_5.Text = RearVerticalPitch._4_5.ToString();
          tRearVerticalPitch_5_6.Text = RearVerticalPitch._5_6.ToString();
    tRearVerticalPitch_6_7.Text = RearVerticalPitch._6_7.ToString();
        tRearVerticalPitch_7_8.Text = RearVerticalPitch._7_8.ToString();
     tRearVerticalPitch_8_9.Text = RearVerticalPitch._8_9.ToString();
         tRearVerticalPitch_9_10.Text = RearVerticalPitch._9_10.ToString();
        tTubeSupportQuantity.Text = TubeSupportQuantity.ToString();
           cTubeSupportSize.Text = TubeSupportSize;
                bCamber.Checked = Cambered;
           cTileblockManuf.Text = TitleblockManuf;
        tPlenumLength.Text = Plenum_Length.ToString();
      tOffsetFromPlenumCenter.Text = OffsetFromCenter.ToString();
         cPlenumStyle.Text = Plenum_Design.ToString();
 cColumnSize.Text = Beam_Size.ToString();
                tFanCount.Text = Fan_Count.ToString();
                tLugStagger.Text = Lug_HPC.Stagger.ToString();
           if (Plenum_Design == Design.Johnson)
    {
     tExtraLength.Enabled = true;
          tExtraLength.Text = Johnson_ExtraLength.ToString();
   }
     else
  {
         tExtraLength.Enabled = false;
      tExtraLength.Text = "";
            }
             tWeight.Text = TotalUnitWeight.ToString();
 if (Developer)
      {
             Lug_HPC.Spacing = Tube.Length * 0.6;
        tLiftingLugSpacing.Text = Lug_HPC.Spacing.ToString();
                }

              // Advanced
            createDrawing_Toggle.Checked = Default.Toggle_CreateDrawing;
        save_Toggle.Checked = Default.Toggle_Save;
    delete_Toggle.Checked = Default.Toggle_DeleteFiles;

                // Job
            textBox_Bank.Text = Bank.ToString();
           job_Box.Text = Project;
   customer_Box.Text = Customer;
   client_Box.Text = Client;
                location_Box.Text = PlantLocation;
   purchaseOrder_Box.Text = PurchaseOrder;
     itemNumber_Box.Text = ItemNumber;
         initials_Box.Text = Initials;

              // Headers
         MapLocal_UI_To_DTO(this);
         LoadHeaderData_FromApp("61");
        LoadHeaderData_FromApp("62");
        LoadHeaderData_FromApp("63");
         LoadHeaderData_FromApp("64");
   LoadHeaderData_FromApp("65");
     LoadHeaderData_FromApp("66");

        GlobalErrorHandler.LogInfo("BundleUI_Load completed successfully");
            }
            catch (Exception ex)
        {
       GlobalErrorHandler.LogError(ex, "BundleUI_Load");
  MessageBox.Show(
     $"Error loading the form:\n\n{ex.Message}\n\nSome features may not work correctly.",
             "Load Error",
   MessageBoxButtons.OK,
             MessageBoxIcon.Warning);
        }
   }

        private void BundleUI_FormClosing(object sender, FormClosingEventArgs e)
        {
      try
          {
                GlobalErrorHandler.LogInfo("BundleUI closing...");

         try
   {
  PleaseWait.Stop();
      }
      catch { }

    _comManager?.Dispose();
          DisconnectSolidWorks();

      GlobalErrorHandler.LogInfo("BundleUI closed successfully");
   }
     catch (Exception ex)
            {
    GlobalErrorHandler.LogError(ex, "BundleUI_FormClosing");
  }
        }

        #endregion
        //private void MapLocal_UI_To_DTO() 
        //{
        //    HeaderAppData = new Dictionary<string, UI_DTO>();

        //    for (int i = 61; i <= 66; i++)
        //    {
        //        var uiDto = new UI_DTO
        //        {
        //            Header = GetHeader(i),
        //            Enabled = GetControl<CheckBox>("cEnabled", i),
        //            BoxWidthTextBox = GetControl<TextBox>("tBoxWidth", i),
        //            TubesheetTHKTextBox = GetControl<TextBox>("tTubesheetTHK_", i),
        //            PlugsheetTHKTextBox = GetControl<TextBox>("tPlugsheetTHK_", i),
        //            TopAndBottomPlateTHKTextBox = GetControl<TextBox>("tTopBottomTHK_", i),
        //            BoxLengthTextBox = GetControl<TextBox>("tBoxLength_", i),
        //            BoxHeightTextBox = GetControl<TextBox>("tVerticalSpan_", i),
        //            WetLocationYTextBox = GetControl<TextBox>("tWetLocationY_", i),
        //            TubeOddXTextBox = GetControl<TextBox>("Xtop_", i)
        //        };

        //        HeaderAppData.Add(i.ToString(), uiDto);
        //    }
        //}
        #region Buttons

        private void bImportPrego_Click(object sender, EventArgs e)
        {
         try
            {
     GlobalErrorHandler.LogInfo("Import Prego clicked");

                if (PregoDoc != null)
             {
  if (!Developer)
   {
      ClearPregoOnJobChanged = false;
 Project = LoadPregoValue<string>(job_Box, InputSheet, "H2");
    Bank = (char)(CellDouble(InputSheet, "C7") + 64);
          textBox_Bank.Text = Bank.ToString();
    ClearPregoOnJobChanged = true;
  }

         TitleblockManuf = LoadPregoValue<string>(cTileblockManuf, InputSheet, "G27", "F27");

  if (!IsSmithco && Tube.SlopesPerFootList[Tube.RowCount] == 0)
        {
                  bCamber.Checked = Cambered = true;
      }
    else
  {
      bCamber.Checked = Cambered = false;
     }

      // Job info
           Customer = LoadPregoValue<string>(customer_Box, InputSheet, "B" + 2);
           Client = LoadPregoValue<string>(client_Box, InputSheet, "B" + 2);
    PlantLocation = LoadPregoValue<string>(location_Box, InputSheet, "B" + 4);
      PurchaseOrder = LoadPregoValue<string>(purchaseOrder_Box, InputSheet, "B" + 5);
       ItemNumber = LoadPregoValue<string>(itemNumber_Box, InputSheet, "H" + 3);

        // Bundle
                 Bundle_Width = LoadPregoDouble(tBundleWidth, InputSheet, "BQ" + 45, "F12");
          if (Bundle_Width < 16)
     {
         Bundle_Width *= 12;
      tBundleWidth.Text = Bundle_Width.ToString();
          }
      HeadersOutsideFrames = LoadPregoBool(cHeadersOutsideFrame, InputSheet, "G" + 15, "F" + 15);
               TubeLength = LoadPregoDouble_FeetToInches(tTubeLength, InputSheet, "L" + 15, "N" + 15);
          TubeProjection = LoadPregoDouble(tTubeProjection, InputSheet, 0.25, 0.125);
  FinStripBack_Front = LoadPregoDouble(tFrontFinStripBack, PregoToMikeySheet, "X39");
    FinStripBack_Rear = LoadPregoDouble(tRearFinStripBack, PregoToMikeySheet, "Y39");
  TubeOD = LoadPregoDouble(tTubeOD, InputSheet, "L10");
       TubeWallTHK = LoadPregoDouble(tTubeWallTHK, InputSheet, "N14", "L14");
FinOD = LoadPregoDouble(tFinOD, InputSheet, "N19", "L19");
    Tube_Row_1L = LoadPregoInt(tTubes_Row_1L, InputSheet, "AW39", "AU39");
    Tube_Row_2L = LoadPregoInt(tTubes_Row_2L, InputSheet, "AW42", "AU42");
      TubeHorizPitch = LoadPregoDouble(tTubeHorizPitch, InputSheet, "BO47");
           TubeQuantity = LoadPregoInt(tTubeQuantity, InputSheet, "N20", "L20");
           FrontVerticalPitch._1_2 = LoadPregoDouble(tFrontVerticalPitch_1_2, SketchCalcsSheet, "DF58");
    FrontVerticalPitch._2_3 = LoadPregoDouble(tFrontVerticalPitch_2_3, SketchCalcsSheet, "DF59");
      FrontVerticalPitch._3_4 = LoadPregoDouble(tFrontVerticalPitch_3_4, SketchCalcsSheet, "DF60");
    FrontVerticalPitch._4_5 = LoadPregoDouble(tFrontVerticalPitch_4_5, SketchCalcsSheet, "DF61");
        FrontVerticalPitch._5_6 = LoadPregoDouble(tFrontVerticalPitch_5_6, SketchCalcsSheet, "DF62");
  FrontVerticalPitch._6_7 = LoadPregoDouble(tFrontVerticalPitch_6_7, SketchCalcsSheet, "DF63");
         FrontVerticalPitch._7_8 = LoadPregoDouble(tFrontVerticalPitch_7_8, SketchCalcsSheet, "DF64");
      FrontVerticalPitch._8_9 = LoadPregoDouble(tFrontVerticalPitch_8_9, SketchCalcsSheet, "DF65");
        FrontVerticalPitch._9_10 = LoadPregoDouble(tFrontVerticalPitch_9_10, SketchCalcsSheet, "DF66");
     RearVerticalPitch._1_2 = LoadPregoDouble(tRearVerticalPitch_1_2, SketchCalcsSheet, "DF70");
             RearVerticalPitch._2_3 = LoadPregoDouble(tRearVerticalPitch_2_3, SketchCalcsSheet, "DF71");
    RearVerticalPitch._3_4 = LoadPregoDouble(tRearVerticalPitch_3_4, SketchCalcsSheet, "DF72");
     RearVerticalPitch._4_5 = LoadPregoDouble(tRearVerticalPitch_4_5, SketchCalcsSheet, "DF73");
        RearVerticalPitch._5_6 = LoadPregoDouble(tRearVerticalPitch_5_6, SketchCalcsSheet, "DF74");
     RearVerticalPitch._6_7 = LoadPregoDouble(tRearVerticalPitch_6_7, SketchCalcsSheet, "DF75");
     RearVerticalPitch._7_8 = LoadPregoDouble(tRearVerticalPitch_7_8, SketchCalcsSheet, "DF76");
   RearVerticalPitch._8_9 = LoadPregoDouble(tRearVerticalPitch_8_9, SketchCalcsSheet, "DF77");
           RearVerticalPitch._9_10 = LoadPregoDouble(tRearVerticalPitch_9_10, SketchCalcsSheet, "DF78");
          TubeSupportSpacing_Feet = LoadPregoDouble(tTubeSupportSpacing_Feet, InputsCalcsSheet, "BGF12");
       TubeSupportQuantity = LoadPregoInt(tTubeSupportQuantity, InputsCalcsSheet, "BGF20");
         TubeSupportSize = LoadPregoValue<string>(cTubeSupportSize, InputSheet, "CG28", "CF28", "CG26", "CF26").Split(' ')[0];

     // SideFrame
SideFrame_Depth = LoadPregoDouble(tDepth, InputsCalcsSheet, "BGM26");
         SideFrame_THK = LoadPregoValue<double>(cSideFrameTHK, InputSheet, "CG" + 32, "CF" + 32, "CG30", "CF30");

      ImportHeaderData_FromPrego();

        GlobalErrorHandler.LogInfo("Prego data imported successfully");
 MessageBox.Show(
     "Data imported from Prego successfully",
         "Success",
       MessageBoxButtons.OK,
              MessageBoxIcon.Information);
       }
         else
                {
          MessageBox.Show(
             "Prego file not found",
"Error",
MessageBoxButtons.OK,
              MessageBoxIcon.Error);
        }
            }
  catch (Exception ex)
          {
  GlobalErrorHandler.LogError(ex, "bImportPrego_Click");
            MessageBox.Show(
            $"Error importing Prego data:\n\n{ex.Message}",
        "Import Error",
           MessageBoxButtons.OK,
         MessageBoxIcon.Error);
            }
        }

        private void bBundle_Click(object sender, EventArgs e)
   {
   try
            {
     GlobalErrorHandler.LogInfo("Bundle button clicked");

    if (!Developer)
      {
  SignInitials();
                }

   if (!Lug_HPC.Spacing.HasValue)
    {
     MessageBox.Show(
         "You must enter a lifting lug spacing to run the bundle automation.",
     "Input Required",
            MessageBoxButtons.OK,
          MessageBoxIcon.Warning);
        return;
           }

             // Check if SolidWorks is available
     if (!IsSolidWorksAvailable())
                {
   var result = MessageBox.Show(
    "SolidWorks is not currently running.\n\n" +
   "The bundle automation requires SolidWorks to be running.\n\n" +
      "Would you like to:\n" +
      "• Click 'Retry' after starting SolidWorks\n" +
 "• Click 'Cancel' to abort",
        "SolidWorks Required",
          MessageBoxButtons.RetryCancel,
       MessageBoxIcon.Warning);

             if (result == DialogResult.Retry)
                {
  ResetConnection();
   if (!IsSolidWorksAvailable())
{
     MessageBox.Show(
       "Still unable to connect to SolidWorks.\n\n" +
       "Please ensure SolidWorks is running and try again.",
            "Connection Failed",
   MessageBoxButtons.OK,
      MessageBoxIcon.Error);
            return;
             }
         }
       else
   {
     return;
    }
     }

           GlobalErrorHandler.LogInfo("Creating bundle...");
         new Bundle(7, "Bundle");
   GlobalErrorHandler.LogInfo("Bundle created successfully");

      MessageBox.Show(
          "Bundle created successfully!",
       "Success",
           MessageBoxButtons.OK,
        MessageBoxIcon.Information);
            }
  catch (InvalidOperationException ex) when (ex.Message.Contains("SolidWorks"))
      {
     GlobalErrorHandler.LogError(ex, "bBundle_Click - SolidWorks error");
           MessageBox.Show(
    $"SolidWorks error:\n\n{ex.Message}",
    "SolidWorks Error",
         MessageBoxButtons.OK,
        MessageBoxIcon.Error);
            }
          catch (Exception ex)
            {
       GlobalErrorHandler.LogError(ex, "bBundle_Click");
             MessageBox.Show(
$"Error creating bundle:\n\n{ex.Message}\n\nCheck the log file for details.",
              "Bundle Creation Error",
   MessageBoxButtons.OK,
      MessageBoxIcon.Error);
  }
        }

        #endregion
        #region Helpers

        private IHeaderExtensions GetHeader(int index)
        {
            switch (index)
            {
                case 61:
                    return Header61;
                case 62:
                    return Header62;
                case 63:
                    return Header63;
                case 64:
                    return Header64;
                case 65:
                    return Header65;
                case 66:
                    return Header66;
                default:
                    throw new ArgumentException($"Invalid index: {index}");
            }
        }
        private T GetControl<T>(string baseName, int index) where T : class
        {
            var controlName = $"{baseName}{index}";
            return this.GetType().GetField(controlName, BindingFlags.Instance | BindingFlags.NonPublic)?.GetValue(this) as T;
        }

        #endregion

        #region UpdateUI

        #region UpdateUI_Headers
        private void Xtop_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeOddX_61.Text, x => Header61.TubeOddX = x);
        }

        private void Xtop_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeOddX_62.Text, x => Header62.TubeOddX = x);
        }
        private void tY_Location_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tWetLocationY_61.Text, x => Header61.TubeY = x);
        }

        private void tY_Location_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tWetLocationY_62.Text, x => Header62.TubeY = x);
        }

        private void tY_Location_63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tWetLocationY_63.Text, x => Header63.TubeY = x);
        }

        private void tY_Location_64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tWetLocationY_64.Text, x => Header64.TubeY = x);
        }

        private void tY_Location_65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tWetLocationY_65.Text, x => Header65.TubeY = x);
        }

        private void tY_Location_66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tWetLocationY_66.Text, x => Header66.TubeY = x);
        }
        private void tTopBottomTHK_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTopBottomTHK_61.Text, x => Header61.TopAndBottomPlateTHK = x);
        }

        private void tTopBottomTHK_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTopBottomTHK_62.Text, x => Header62.TopAndBottomPlateTHK = x);
        }

        private void tTopBottomTHK_63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTopBottomTHK_63.Text, x => Header63.TopAndBottomPlateTHK = x);
        }

        private void tTopBottomTHK_64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTopBottomTHK_64.Text, x => Header64.TopAndBottomPlateTHK = x);
        }

        private void tTopBottomTHK_65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTopBottomTHK_65.Text, x => Header65.TopAndBottomPlateTHK = x);
        }

        private void tTopBottomTHK_66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTopBottomTHK_66.Text, x => Header66.TopAndBottomPlateTHK = x);
        }
        private void tBoxLength_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxLength_61.Text, x => Header61.BoxLength = x);
        }

        private void tBoxLength_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxLength_62.Text, x => Header62.BoxLength = x);
        }

        private void tBoxLength_63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxLength_63.Text, x => Header63.BoxLength = x);
        }

        private void tBoxLength_64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxLength_64.Text, x => Header64.BoxLength = x);
        }

        private void tBoxLength_65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxLength_65.Text, x => Header65.BoxLength = x);
        }

        private void tBoxLength_66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxLength_66.Text, x => Header66.BoxLength = x);
        }
        private void tVerticalSpan_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxHeight_61.Text, x => Header61.BoxHeight = x);
        }

        private void tVerticalSpan_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxHeight_62.Text, x => Header62.BoxHeight = x);
        }

        private void tVerticalSpan_63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxHeight_63.Text, x => Header63.BoxHeight = x);
        }

        private void tVerticalSpan_64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxHeight_64.Text, x => Header64.BoxHeight = x);
        }

        private void tVerticalSpan_65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxHeight_65.Text, x => Header65.BoxHeight = x);
        }

        private void tVerticalSpan_66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxHeight_66.Text, x => Header66.BoxHeight = x);
        }
        private void cEnabled61_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled61.Checked, x => Header61.IsRequired = x);
            LoadHeaderData_FromApp("61");
        }

        private void cEnabled62_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled62.Checked, x => Header62.IsRequired = x);
            LoadHeaderData_FromApp("62");
        }

        private void cEnabled63_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled63.Checked, x => Header63.IsRequired = x);
            LoadHeaderData_FromApp("63");
        }

        private void cEnabled64_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled64.Checked, x => Header64.IsRequired = x);
            LoadHeaderData_FromApp("64"); ;
        }

        private void cEnabled65_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled65.Checked, x => Header65.IsRequired = x);
            LoadHeaderData_FromApp("65");
        }

        private void cEnabled66_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cEnabled66.Checked, x => Header66.IsRequired = x);
            LoadHeaderData_FromApp("66");
        }
        private void tBoxWidth61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxWidth_61.Text, x => Header61.BoxWidth = x);
        }

        private void tBoxWidth62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxWidth_62.Text, x => Header62.BoxWidth = x);
        }

        private void tBoxWidth63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxWidth_63.Text, x => Header63.BoxWidth = x);
        }

        private void tBoxWidth64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxWidth_64.Text, x => Header64.BoxWidth = x);
        }

        private void tBoxWidth65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxWidth_65.Text, x => Header65.BoxWidth = x);
        }

        private void tBoxWidth66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBoxWidth_66.Text, x => Header66.BoxWidth = x);
        }
        private void tTubesheetTHK_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubesheetTHK_61.Text, x => Header61.TubesheetTHK = x);
        }

        private void tTubesheetTHK_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubesheetTHK_62.Text, x => Header62.TubesheetTHK = x);
        }

        private void tTubesheetTHK_63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubesheetTHK_63.Text, x => Header63.TubesheetTHK = x);
        }

        private void tTubesheetTHK_64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubesheetTHK_64.Text, x => Header64.TubesheetTHK = x);
        }

        private void tTubesheetTHK_65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubesheetTHK_65.Text, x => Header65.TubesheetTHK = x);
        }

        private void tTubesheetTHK_66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubesheetTHK_66.Text, x => Header66.TubesheetTHK = x);
        }
        private void tPlugsheetTHK_61_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlugsheetTHK_61.Text, x => Header61.PlugsheetTHK = x);
        }

        private void tPlugsheetTHK_62_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlugsheetTHK_62.Text, x => Header62.PlugsheetTHK = x);
        }

        private void tPlugsheetTHK_63_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlugsheetTHK_63.Text, x => Header63.PlugsheetTHK = x);
        }

        private void tPlugsheetTHK_64_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlugsheetTHK_64.Text, x => Header64.PlugsheetTHK = x);
        }

        private void tPlugsheetTHK_65_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlugsheetTHK_65.Text, x => Header65.PlugsheetTHK = x);
        }

        private void tPlugsheetTHK_66_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlugsheetTHK_66.Text, x => Header66.PlugsheetTHK = x);
        }

        #endregion
        #region UpdateUI_Bundle
        private void tRearVerticalPitch_1_2_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tRearVerticalPitch_1_2.Text, x => RearVerticalPitch._1_2 = x);
        }

        private void tRearVerticalPitch_2_3_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tRearVerticalPitch_2_3.Text, x => RearVerticalPitch._2_3 = x);
        }

        private void tRearVerticalPitch_3_4_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tRearVerticalPitch_3_4.Text, x => RearVerticalPitch._3_4 = x);
        }

        private void tRearVerticalPitch_4_5_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tRearVerticalPitch_4_5.Text, x => RearVerticalPitch._4_5 = x);
        }

        private void tRearVerticalPitch_5_6_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tRearVerticalPitch_5_6.Text, x => RearVerticalPitch._5_6 = x);
        }

        private void tRearVerticalPitch_6_7_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tRearVerticalPitch_6_7.Text, x => RearVerticalPitch._6_7 = x);
        }

        private void tRearVerticalPitch_7_8_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tRearVerticalPitch_7_8.Text, x => RearVerticalPitch._7_8 = x);
        }

        private void tRearVerticalPitch_8_9_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tRearVerticalPitch_8_9.Text, x => RearVerticalPitch._8_9 = x);
        }

        private void tRearVerticalPitch_9_10_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tRearVerticalPitch_9_10.Text, x => RearVerticalPitch._9_10 = x);
        }
        private void tTileblockManuf_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(cTileblockManuf.Text, x => TitleblockManuf = x);
        }
        private void bCamber_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(bCamber.Checked, x => Cambered = x);
        }
        private void tTubeSupportSpacing_Feet_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeSupportSpacing_Feet.Text, x => TubeSupportSpacing_Feet = x);
        }

        private void tTubeSupportQuantity_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeSupportQuantity.Text, x => TubeSupportQuantity = x);
        }

        private void cTubeSupportSize_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(cTubeSupportSize.Text, x => TubeSupportSize = x);
        }
        private void tVerticalPitch_1_2_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFrontVerticalPitch_1_2.Text, x => FrontVerticalPitch._1_2 = x);
        }

        private void tVerticalPitch_2_3_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFrontVerticalPitch_2_3.Text, x => FrontVerticalPitch._2_3 = x);
        }

        private void tVerticalPitch_3_4_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFrontVerticalPitch_3_4.Text, x => FrontVerticalPitch._3_4 = x);
        }

        private void tVerticalPitch_4_5_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFrontVerticalPitch_4_5.Text, x => FrontVerticalPitch._4_5 = x);
        }

        private void tVerticalPitch_5_6_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFrontVerticalPitch_5_6.Text, x => FrontVerticalPitch._5_6 = x);
        }

        private void tVerticalPitch_6_7_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFrontVerticalPitch_6_7.Text, x => FrontVerticalPitch._6_7 = x);
        }

        private void tVerticalPitch_7_8_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFrontVerticalPitch_7_8.Text, x => FrontVerticalPitch._7_8 = x);
        }

        private void tVerticalPitch_8_9_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFrontVerticalPitch_8_9.Text, x => FrontVerticalPitch._8_9 = x);
        }

        private void tVerticalPitch_9_10_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFrontVerticalPitch_9_10.Text, x => FrontVerticalPitch._9_10 = x);
        }
        private void tTubeQuantity_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(tTubeQuantity.Text, x => TubeQuantity = x);
        }
        private void tHorizPitch_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeHorizPitch.Text, x => TubeHorizPitch = x);
        }
        private void tTubes_Row_1L_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(tTubes_Row_1L.Text, x => Tube_Row_1L = x);
        }

        private void tTubes_Row_2L_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(tTubes_Row_2L.Text, x => Tube_Row_2L = x);
        }
        private void tTubeOD_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeOD.Text, x => TubeOD = x);
        }

        private void tTubeWallTHK_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeWallTHK.Text, x => TubeWallTHK = x);
        }

        private void tFinOD_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFinOD.Text, x => FinOD = x);
        }
        private void tFrontFinStripBack_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tFrontFinStripBack.Text, x => FinStripBack_Front = x);
        }

        private void tRearFinStripBack_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tRearFinStripBack.Text, x => FinStripBack_Rear = x);
        }
        private void tTubeLength_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeLength.Text, x => TubeLength = x);
        }

        private void tTubeProjection_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tTubeProjection.Text, x => TubeProjection = x);
        }
        private void cHeadersOutsideFrame_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(cHeadersOutsideFrame.Checked, x => HeadersOutsideFrames = x);
        }
        private void tWidth_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tBundleWidth.Text, x => Bundle_Width = x);
        }

        private void cSideFrameTHK_Leave(object sender, EventArgs e)
        {
            cSideFrameTHK.Text = SideFrame_THK.ToString();
        }
        private void tDepth_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tDepth.Text, x => SideFrame_Depth = x);
        }

        private void textBox_Bank_TextChanged(object sender, EventArgs e)
        {
            string text = textBox_Bank.Text.ToUpper();
            textBox_Bank.Text = text;
            UI_CharChanged(text, x => Bank = x);

            if (!Developer)
            {
                Lug_HPC.Spacing = null;
                tLiftingLugSpacing.Text = "";
            }
        }
        private void job_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(job_Box.Text, x => Project = x);

            if (!Developer)
            {
                Lug_HPC.Spacing = null;
                tLiftingLugSpacing.Text = "";
            }
        }

        private void customer_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(customer_Box.Text, x => Customer = x);
        }

        private void client_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(client_Box.Text, x => Client = x);
        }

        private void location_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(location_Box.Text, x => PlantLocation = x);
        }

        private void purchaseOrder_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(purchaseOrder_Box.Text, x => PurchaseOrder = x);
        }

        private void itemNumber_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(itemNumber_Box.Text, x => ItemNumber = x);
        }

        private void initials_Box_TextChanged(object sender, EventArgs e)
        {
            UI_StringChanged(initials_Box.Text, x => Initials = x);
        }

        #endregion
        #region UpdateUI_AdvancedTab

        private void createDrawing_Toggle_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(createDrawing_Toggle.Checked, x => Default.Toggle_CreateDrawing = x);
        }

        private void save_Toggle_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(save_Toggle.Checked, x => Default.Toggle_Save = x);
        }

        private void delete_Toggle_CheckedChanged(object sender, EventArgs e)
        {
            UI_BoolChanged(delete_Toggle.Checked, x => Default.Toggle_DeleteFiles = x);
        }





        #endregion
        #region UpdateUI_Manual
        private void tLugStagger_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tLugStagger.Text, x => Lug_HPC.Stagger = x);
        }
        private void tFanCount_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(tFanCount.Text, x => Fan_Count = x);
        }
        private void tLiftingLugSpacing_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tLiftingLugSpacing.Text, x => Lug_HPC.Spacing = x);
        }
        private void tWeight_TextChanged(object sender, EventArgs e)
        {
            UI_IntChanged(tWeight.Text, x => TotalUnitWeight = x);
        }
        private void tPlenumLength_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tPlenumLength.Text, x => Plenum_Length = x);
        }

        private void tOffsetFromPlenumCenter_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tOffsetFromPlenumCenter.Text, x => OffsetFromCenter = x);
        }

        private void cPlenumStyle_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (Enum.TryParse(cPlenumStyle.Text, out Design design))
            {
                Plenum_Design = design;
                cPlenumStyle.Text = design.ToString();
                SaveSettings();
            }
            else throw new ArgumentException("Invalid design");

            if (Plenum_Design == Design.Johnson)
            {
                tExtraLength.Enabled = true;
                tExtraLength.Text = Johnson_ExtraLength.ToString();
            }
            else
            {
                tExtraLength.Enabled = false;
                tExtraLength.Text = "";
            }
        }

        private void cColumnSize_SelectedIndexChanged(object sender, EventArgs e)
        {
            UI_StringChanged(cColumnSize.Text, x => Beam_Size = x);
        }

        private void tExtraLength_TextChanged(object sender, EventArgs e)
        {
            UI_DoubleChanged(tExtraLength.Text, x => Johnson_ExtraLength = x);
        }



        #endregion

        #endregion

        private void bExcel_Click(object sender, EventArgs e)
{
            try
         {
        GlobalErrorHandler.LogInfo("Excel cleanup requested");
     Prego.CleanUp(true);
      GlobalErrorHandler.LogInfo("Excel COM cleanup completed");

                MessageBox.Show(
      "Excel cleanup completed successfully.",
   "Success",
      MessageBoxButtons.OK,
      MessageBoxIcon.Information);
            }
            catch (Exception ex)
  {
         GlobalErrorHandler.LogError(ex, "bExcel_Click");
     MessageBox.Show(
          $"Error cleaning up Excel:\n\n{ex.Message}",
   "Cleanup Error",
          MessageBoxButtons.OK,
          MessageBoxIcon.Warning);
            }
 }
    }
}

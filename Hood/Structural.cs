using System.Data;

namespace Hood
{
    internal static class Structural
    {
        // Each table represents the panel web height limits for a given Wind Load and
        // center to center spacing of the stiffener.
        static float[,,] DataSet = new float[5, 15, 5]
        {
                { // Table index 0
                  // 10 GA plate
                  // L 2 X 2 X 3/16 stiffeners
                  //  0   1   2   3   4   --- Index
                  //  24" 30" 36" 42" 48" <-- Max stiffener spacing
                    { 94, 84, 76, 71, 66 }, // Wind Load = 30 --- Index: 0
                    { 87, 77, 71, 65, 61 }, // Wind Load = 35 --- Index: 1
                    { 81, 72, 66, 61, 57 }, // Wind Load = 40 --- Index: 2
                    { 76, 68, 62, 58, 54 }, // Wind Load = 45 --- Index: 3
                    { 72, 65, 59, 55, 51 }, // Wind Load = 50 --- Index: 4
                    { 69, 62, 56, 52, 49 }, // Wind Load = 55 --- Index: 5
                    { 66, 59, 54, 50, 47 }, // Wind Load = 60 --- Index: 6
                    { 64, 57, 52, 48, 45 }, // Wind Load = 65 --- Index: 7
                    { 61, 55, 50, 46, 43 }, // Wind Load = 70 --- Index: 8
                    { 59, 53, 48, 45, 42 }, // Wind Load = 75 --- Index: 9
                    { 57, 51, 47, 43, 41 }, // Wind Load = 80 --- Index: 10
                    { 56, 50, 45, 42, 39 }, // Wind Load = 85 --- Index: 11
                    { 54, 48, 44, 41, 38 }, // Wind Load = 90 --- Index: 12
                    { 53, 47, 43, 40, 37 }, // Wind Load = 95 --- Index: 13
                    { 51, 46, 42, 39, 36 }  // Wind Load = 100 -- Index: 14
                },
                { // Table index 1
                  // 10 GA plate
                  // L 2.5 X 2.5 X 3/16 stiffeners
                  //  0   1   2   3   4   --- Index
                  //  24" 30" 36" 42" 48" <-- Max stiffener spacing
                    { 99, 89, 81, 75, 70 }, // Wind Load = 30 --- Index: 0
                    { 92, 82, 75, 69, 65 }, // Wind Load = 35 --- Index: 1
                    { 86, 77, 70, 65, 61 }, // Wind Load = 40 --- Index: 2
                    { 81, 72, 66, 61, 57 }, // Wind Load = 45 --- Index: 3
                    { 77, 69, 63, 58, 54 }, // Wind Load = 50 --- Index: 4
                    { 73, 65, 60, 55, 52 }, // Wind Load = 55 --- Index: 5
                    { 70, 63, 57, 53, 50 }, // Wind Load = 60 --- Index: 6
                    { 67, 60, 55, 51, 48 }, // Wind Load = 65 --- Index: 7
                    { 65, 58, 53, 49, 46 }, // Wind Load = 70 --- Index: 8
                    { 63, 56, 51, 47, 44 }, // Wind Load = 75 --- Index: 9
                    { 61, 54, 50, 46, 43 }, // Wind Load = 80 --- Index: 10
                    { 59, 53, 48, 45, 42 }, // Wind Load = 85 --- Index: 11
                    { 57, 51, 47, 43, 40 }, // Wind Load = 90 --- Index: 12
                    { 56, 50, 45, 42, 39 }, // Wind Load = 95 --- Index: 13
                    { 54, 49, 44, 41, 38 }  // Wind Load = 100 -- Index: 14
                },
                { // Table index 2
                  // 3/16 inch plate
                  // L 2 X 2 X 3/16 stiffeners
                  //  0   1   2   3   4   --- Index
                  //  24" 30" 36" 42" 48" <-- Max stiffener spacing
                    { 98, 87, 80, 74, 69 }, // Wind Load = 30 --- Index: 0
                    { 90, 81, 74, 68, 64 }, // Wind Load = 35 --- Index: 1
                    { 85, 76, 69, 64, 60 }, // Wind Load = 40 --- Index: 2
                    { 80, 71, 65, 60, 56 }, // Wind Load = 45 --- Index: 3
                    { 76, 68, 62, 57, 53 }, // Wind Load = 50 --- Index: 4
                    { 72, 64, 59, 54, 51 }, // Wind Load = 55 --- Index: 5
                    { 69, 62, 56, 52, 49 }, // Wind Load = 60 --- Index: 6
                    { 66, 59, 54, 50, 47 }, // Wind Load = 65 --- Index: 7
                    { 64, 57, 52, 48, 45 }, // Wind Load = 70 --- Index: 8
                    { 62, 55, 50, 47, 44 }, // Wind Load = 75 --- Index: 9
                    { 60, 53, 49, 45, 42 }, // Wind Load = 80 --- Index: 10
                    { 58, 52, 47, 44, 41 }, // Wind Load = 85 --- Index: 11
                    { 56, 50, 46, 43, 40 }, // Wind Load = 90 --- Index: 12
                    { 55, 49, 45, 41, 39 }, // Wind Load = 95 --- Index: 13
                    { 53, 48, 44, 40, 38 }  // Wind Load = 100 -- Index: 14
                },
                { // Table index 3
                  // 3/16 inch plate
                  // L 2.5 X 2.5 X 3/16 stiffeners
                  //  0    1    2   3   4   --- Index
                  //  24"  30"  36" 42" 48" <-- Max stiffener spacing
                    { 120, 108, 98, 91, 85 }, // Wind Load = 30 --- Index: 0
                    { 112, 100, 91, 84, 79 }, // Wind Load = 35 --- Index: 1
                    { 104, 93,  85, 79, 74 }, // Wind Load = 40 --- Index: 2
                    { 98,  88,  80, 74, 70 }, // Wind Load = 45 --- Index: 3
                    { 93,  83,  76, 71, 66 }, // Wind Load = 50 --- Index: 4
                    { 89,  80,  73, 67, 63 }, // Wind Load = 55 --- Index: 5
                    { 85,  76,  70, 64, 60 }, // Wind Load = 60 --- Index: 6
                    { 82,  73,  67, 62, 58 }, // Wind Load = 65 --- Index: 7
                    { 79,  71,  64, 60, 56 }, // Wind Load = 70 --- Index: 8
                    { 76,  68,  62, 58, 54 }, // Wind Load = 75 --- Index: 9
                    { 74,  66,  60, 56, 52 }, // Wind Load = 80 --- Index: 10
                    { 72,  64,  58, 54, 51 }, // Wind Load = 85 --- Index: 11
                    { 70,  62,  57, 53, 49 }, // Wind Load = 90 --- Index: 12
                    { 68,  61,  55, 51, 48 }, // Wind Load = 95 --- Index: 13
                    { 66,  59,  54, 50, 47 }  // Wind Load = 100 -- Index: 14
                },
                { // Table index 4
                  // 1/4 inch plate
                  // 2.5 X 2.5 X 1/4 stiffeners
                  //  0    1    2    3   4   --- Index
                  //  24"  30"  36"  42" 48" <-- Max stiffener spacing
                    { 127, 113, 103, 96, 90 }, // Wind Load = 30 --- Index: 0
                    { 117, 105, 96,  89, 83 }, // Wind Load = 35 --- Index: 1
                    { 110, 98,  90,  83, 78 }, // Wind Load = 40 --- Index: 2
                    { 103, 93,  84,  78, 73 }, // Wind Load = 45 --- Index: 3
                    { 98,  88,  80,  74, 69 }, // Wind Load = 50 --- Index: 4
                    { 94,  84,  76,  71, 66 }, // Wind Load = 55 --- Index: 5
                    { 90,  80,  73,  68, 63 }, // Wind Load = 60 --- Index: 6
                    { 86,  77,  70,  65, 61 }, // Wind Load = 65 --- Index: 7
                    { 83,  74,  68,  63, 59 }, // Wind Load = 70 --- Index: 8
                    { 80,  72,  65,  61, 57 }, // Wind Load = 75 --- Index: 9
                    { 78,  69,  63,  59, 55 }, // Wind Load = 80 --- Index: 10
                    { 75,  67,  61,  57, 53 }, // Wind Load = 85 --- Index: 11
                    { 73,  65,  60,  55, 52 }, // Wind Load = 90 --- Index: 12
                    { 71,  64,  58,  54, 50 }, // Wind Load = 95 --- Index: 13
                    { 69,  62,  57,  52, 49 }  // Wind Load = 100 -- Index: 14
                },
        };

        public static void SetPanelAndStiffenerStrength(HoodData.IStiffener stiffener, HoodData.IPanel panel)
        {
            int iTable = 0;
            int iWind = (int)(HoodData.WindLoad - 30) / 5;
            int iPanel = 4;
            float lastPanel;

            do
            {
                lastPanel = DataSet[iTable, iWind, iPanel];

                if (iTable != 4)
                {
                    if (iPanel != 0)
                    {
                        iPanel--;
                    }
                    else
                    {
                        iPanel = 4;
                        iTable++;
                    }
                }
                else
                {
                    System.Windows.Forms.MessageBox.Show("Hood is too large for specified wind load");
                    iTable = 4;
                    iPanel = 0;
                    break;
                }

            } while (panel.WebHeight > lastPanel);

            switch (iTable)
            {
                case 0:
                    stiffener.Leg = 2;
                    stiffener.THK = 0.1875;
                    stiffener.R = 0.4375;
                    stiffener.Gauge = 1.125;
                    stiffener.JDEnumber = "54969-HPC";
                    stiffener.Description = "ANGLE_2\"x2\"x3/16\"_A572-50";

                    panel.THK = 0.1344;
                    panel.JDEnumber = "11110-HPC";
                    panel.Description = "SHEET_10GA_A1011-33";
                    break;
                case 1:
                    stiffener.Leg = 2.5;
                    stiffener.THK = 0.1875;
                    stiffener.R = 0.5;
                    stiffener.Gauge = 1.375;
                    stiffener.JDEnumber = "54888";
                    stiffener.Description = "ANGLE_2-1/2\"x2-1/2\"x3/16\"_A572-50";

                    panel.THK = 0.1344;
                    panel.JDEnumber = "11110-HPC";
                    panel.Description = "SHEET_10GA_A1011-33";
                    break;
                case 2:
                    stiffener.Leg = 2;
                    stiffener.THK = 0.1875;
                    stiffener.R = 0.4375;
                    stiffener.Gauge = 1.125;
                    stiffener.JDEnumber = "54969-HPC";
                    stiffener.Description = "ANGLE_2\"x2\"x3/16\"_A572-50";

                    panel.THK = 0.1875;
                    panel.JDEnumber = "60015-HPC";
                    panel.Description = "PLATE_3/16\"_A572_50";
                    break;
                case 3:
                    stiffener.Leg = 2.5;
                    stiffener.THK = 0.1875;
                    stiffener.R = 0.5;
                    stiffener.Gauge = 1.375;
                    stiffener.JDEnumber = "54888";
                    stiffener.Description = "ANGLE_2-1/2\"x2-1/2\"x3/16\"_A572-50";

                    panel.THK = 0.1875;
                    panel.JDEnumber = "60015-HPC";
                    panel.Description = "PLATE_3/16\"_A572_50";
                    break;
                case 4:
                    stiffener.Leg = 2.5;
                    stiffener.THK = 0.25;
                    stiffener.R = 0.5625;
                    stiffener.Gauge = 1.375;
                    stiffener.JDEnumber = "53499-HPC";
                    stiffener.Description = "ANGLE_2-1/2\"x2-1/2\"x1/4\"_A572-50";

                    panel.THK = 0.25;
                    panel.JDEnumber = "60038";
                    panel.Description = "PLATE_1/4\"_A572_50";
                    break;
            }

            switch (iPanel)
            {
                case 4:
                    stiffener.MaxSpacing = 48;
                    break;
                case 3:
                    stiffener.MaxSpacing = 42;
                    break;
                case 2:
                    stiffener.MaxSpacing = 36;
                    break;
                case 1:
                    stiffener.MaxSpacing = 30;
                    break;
                case 0:
                    stiffener.MaxSpacing = 24;
                    break;
            }
        }
    }
}

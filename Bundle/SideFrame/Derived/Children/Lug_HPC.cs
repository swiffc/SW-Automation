using Bundle.SideFrame.Derived.Children;
using FileTools.Base;
using ModelTools;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using static FileTools.CommonData.CommonData;
using static FileTools.StaticFileTools;

namespace Bundle.SideFrame.Derived.Children
{
    internal class Lug_HPC : Part
    {
        // Static properties

        static public (double width, double THK, double height) Dim
        {
            get
            {
                // Find the smallest key in UnitWeight that is greater than or equal to TotalUnitWeight
                var nextHighestWeight = UnitWeight.Keys
                                                   .Where(key => key >= TotalUnitWeight)
                                                   .DefaultIfEmpty(-1) // This ensures there's a value if no key is found
                                                   .Min();

                return UnitWeight[nextHighestWeight];
            }
        }
        static bool ShowMessage { get; set; } = true;
        bool IsEnabled
        {
            get
            {
                // Find the smallest key in UnitWeight that is greater than or equal to TotalUnitWeight
                var nextHighestWeight = UnitWeight.Keys
                                                   .Where(key => key >= TotalUnitWeight)
                                                   .DefaultIfEmpty(-1) // This ensures there's a value if no key is found
                                                   .Min();

                if (nextHighestWeight == -1)
                {
                    if (ShowMessage)
                    {
                        MessageBox.Show("Unit is over 95,000 lbs. and requires a lifting capacity evaluation.", "Weight Overload!", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                        ShowMessage = false;
                    }
                    
                    return false;
                }
                return true;
            }
        }
        static public double? Spacing { get; set; }
        static public double Stagger { get; set; } = 16;


        // Constructor
        public Lug_HPC(SubAssembly parentSubAssembly) : base(parentSubAssembly) 
        { 
            // Show message only once
            if (ParentSubAssembly is SideFrameWeldmentLeft)
                ShowMessage = false;

            // Allows for the same part to be used in multiple assemblies
            AssignedComponentPaths.Remove(FilePath);
        }


        // Property overrides
        public override bool Enabled => IsEnabled;
        public override string StaticPartNo => "1300HPC";
        public override Shape RawMaterialShape => Shape.Plate;
        public override string SizeOrThickness => Dim.THK.ToString();
        public override List<PositionData> Position
        {
            get
            {
                double xTranslation = 0;
                double yTranslation = SideFramePart.Depth;
                double zTranslation = (double)Spacing/2;

                return new List<PositionData>
                {
                    PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation),
                    PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation - Stagger),
                };
            }
        }


        // Dictionaries
        static public Dictionary<double, (double width, double THK, double height)> UnitWeight = 
                  new Dictionary<double, (double width, double THK, double height)>
        {//   weight    width   THK     height
            { 37000, (  5.5,    0.75,   9     )   },
            { 56000, (  7,      0.75,   9.75  )   },
            { 67900, (  8,      0.75,   10.25 )   },
            { 71100, (  7,      1,      9.75  )   },
            { 95000, (  8,      1,      10.25 )   },
        };
    }
}

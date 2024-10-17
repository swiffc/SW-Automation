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
    // Represents a Lug_HPC part used in a side frame weldment assembly.
    internal class Lug_HPC : Part
    {
        // Static properties

        // Returns the dimensions (width, THK, height) based on the total unit weight.
        // The smallest key in UnitWeight that is >= TotalUnitWeight is selected.
        static public (double width, double THK, double height) Dim
        {
            get
            {
                // Find the smallest key in UnitWeight that is greater than or equal to TotalUnitWeight.
                var nextHighestWeight = UnitWeight.Keys
                                                   .Where(key => key >= TotalUnitWeight)
                                                   .DefaultIfEmpty(-1) // Default value if no key is found.
                                                   .Min();

                // Return corresponding dimensions (width, THK, height) for the weight.
                return UnitWeight[nextHighestWeight];
            }
        }

        // Controls whether to show a weight overload message box. Defaults to true.
        static bool ShowMessage { get; set; } = true;

        // Property to check if the part is enabled based on total weight.
        // If total weight exceeds 95,000 lbs, shows a message box and disables the part.
        bool IsEnabled
        {
            get
            {
                // Find the smallest key in UnitWeight that is greater than or equal to TotalUnitWeight.
                var nextHighestWeight = UnitWeight.Keys
                                                   .Where(key => key >= TotalUnitWeight)
                                                   .DefaultIfEmpty(-1)
                                                   .Min();

                // If weight exceeds the max defined (95000 lbs), show a warning message once.
                if (nextHighestWeight == -1)
                {
                    if (ShowMessage)
                    {
                        MessageBox.Show("Unit is over 95,000 lbs. and requires a lifting capacity evaluation.", "Weight Overload!", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                        ShowMessage = false;
                    }
                    return false; // Disable part.
                }
                return true; // Part is enabled.
            }
        }

        // Static property to set spacing for the lug. Can be nullable.
        static public double? Spacing { get; set; }

        // Static property for staggering, default set to 16.
        static public double Stagger { get; set; } = 16;

        // Constructor for Lug_HPC
        public Lug_HPC(SubAssembly parentSubAssembly) : base(parentSubAssembly)
        {
            // If the parent subassembly is a SideFrameWeldmentLeft, prevent showing message multiple times.
            if (ParentSubAssembly is SideFrameWeldmentLeft)
                ShowMessage = false;

            // Remove the assigned component path to allow this part to be used in multiple assemblies.
            AssignedComponentPaths.Remove(FilePath);
        }

        // Overrides Part's Enabled property. Returns if the part is enabled or not based on IsEnabled.
        public override bool Enabled => IsEnabled;

        // Overrides the StaticPartNo to return a fixed part number for this part.
        public override string StaticPartNo => "1300HPC";

        // Overrides the RawMaterialShape, returning the part's material shape as a plate.
        public override Shape RawMaterialShape => Shape.Plate;

        // Overrides SizeOrThickness to return the thickness (THK) from Dim.
        public override string SizeOrThickness => Dim.THK.ToString();

        // Overrides the Position property, calculating part positions based on spacing and stagger.
        public override List<PositionData> Position
        {
            get
            {
                // Translation values for positioning the part.
                double xTranslation = 0;
                double yTranslation = SideFramePart.Depth;
                double zTranslation = (double)Spacing / 2;

                // Creates two positions for the lug with stagger and spacing adjustments.
                return new List<PositionData>
                {
                    PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: zTranslation),
                    PositionData.Create(tX: xTranslation, tY: yTranslation, tZ: -zTranslation - Stagger),
                };
            }
        }

        // Dictionary mapping weight to part dimensions (width, THK, height).
        static public Dictionary<double, (double width, double THK, double height)> UnitWeight =
            new Dictionary<double, (double width, double THK, double height)>
        {//    weight    width    THK     height
            { 37000, (  5.5,     0.75,   9     ) },
            { 56000, (  7,       0.75,   9.75  ) },
            { 67900, (  8,       0.75,   10.25 ) },
            { 71100, (  7,       1,      9.75  ) },
            { 95000, (  8,       1,      10.25 ) },
        };
    }
}

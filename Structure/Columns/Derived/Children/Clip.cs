using FileTools.Base;
using ModelTools;
using Structure.Braces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.SharedProperties;

namespace Structure.Columns.Derived.Children
{
    internal abstract class Clip : Part
    {
        // Static properties
        private static double _holeToEdge = 2.0;
        #region HoleToEdge Rules

        public static double HoleToEdge
        {
            get { return _holeToEdge; }
            private set { _holeToEdge = value; } // Make the setter private
        }

        #endregion
        private static double _holeDiameter = 0.8125;
        #region HoleDiameter Rules

        public static double HoleDiameter
        {
            get { return _holeDiameter; }
            set
            {
                _holeDiameter = value;
                HoleToEdge = _holeDiameter > 1 ? 2.5 : 2.0;
            }
        }

        #endregion
        public static double ColumnExtentToHole => 2.0;
        static public double THK { get; set; } = 0.25;


        // Constructor
        protected Clip(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Diameter", "sk:Plate", HoleDiameter);
            EditDimension("HoleToEdge", "sk:Plate", HoleToEdge);
            EditDimension("Angle", "sk:Plate", BraceAngle);
        }

    }
}

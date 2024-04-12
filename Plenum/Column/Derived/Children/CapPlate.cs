using ModelTools;
using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using cTools = ModelTools.ReleaseCOM;
using Ftools = FileTools.FileTools;
using mTools = Tools.ModelTools;
using aTools = ModelTools.AssemblyTools;
using System;
using static Plenum.Plenum;
using System.Data.Common;
using static FileTools.FileTools;

namespace Plenum
{
    internal class CapPlate : Part
    {
        // Static properties
        public static bool Enabled { get; set; } = true;
        internal static AssemblyDoc CallerDoc { get; set; }
        internal static double THK => 0.5;


        // Constructor
        public CapPlate(CallerType callerType) : base(callerType) 
        {
            ChildInstances.Add(this);
        }


        // Private Methods
        protected override void EditDimensions(ModelDoc2 modelDoc2)
        {
            mTools.EditDimension("Length", "sk:Plate", CallerType == CallerType.Legacy ? Width : Length, modelDoc2);
            mTools.EditDimension("Width", "sk:Plate", CallerType == CallerType.Legacy ? Length : Width, modelDoc2);
            mTools.EditDimension("THK", "Plate", THK, modelDoc2);
            mTools.EditDimension("HoleDiameter", "sk:AnchorHole", HoleDiameter, modelDoc2);
            mTools.EditDimension("X", "sk:AnchorHole", HoleSpacingOnWidth / 2, modelDoc2);
            mTools.EditDimension("Y", "sk:AnchorHole", HoleSpacingOnLength / 2, modelDoc2);
        }


        // Property overrides
        public override List<PositionData> Position
        {
            get
            {
                if (_position == null)
                {
                    double yRotation = CallerType == CallerType.Legacy ? 90 : 0;
                    return new List<PositionData> { PositionData.Create(tY: -PlenumColumn.Height, rY: yRotation) };
                }
                return _position;
            }
        }
        public override string StaticPartNo => "103";
        protected override AssemblyDoc ParentAssembly => CallerDoc;
        public override RawMaterial Shape => RawMaterial.Plate;
        public override string Size => THK.ToString();


        // Internal properties
        internal double Length
        {
            get
            {
                return Beam.Depth;
            }
        }
        internal double Width
        {
            get
            {
                return Beam.FlangeWidth;
            }
        }
        internal double HoleDiameter => 0.8125;
        internal double HoleSpacingOnWidth
        {
            get
            {
                if (SteelBook.W_Shape.TryGetValue(PlenumColumn.Size, out var wShape))
                {
                    return wShape.Depth < 7.5 ? 3.5 : 4.5;
                }
                else
                {
                    return 3.5;
                }
            }
            set { }
        }
        internal double HoleSpacingOnLength
        {
            get
            {
                if (SteelBook.W_Shape.TryGetValue(PlenumColumn.Size, out var wShape))
                {
                    return wShape.FlangeWidth < 7.5 ? 3.5 : 4.5;
                }
                else
                {
                    return 3.5;
                }
            }
            set { }
        }
    }
}

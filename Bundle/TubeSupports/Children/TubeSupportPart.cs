using Bundle.SideFrame.Derived.Children;
using FileTools.Base;
using ModelTools;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.CommonData.CommonData;
using static FileTools.StaticFileTools;
using static ModelTools.BendTable;

namespace Bundle.TubeSupports.Children
{
    internal class TubeSupportPart : Part
    {
        // Static properties
        static public double Height => Dims[Size].Height;
        static public double THK => Dims[Size].THK;
        static public double Length
        {
            get
            {
                double length = Bundle.Width - SideFramePart.THK * 2;
                if (IsSmithco)
                    return length -= GetBendRadius(SideFramePart.THK) * 2;
                else
                    return length -= TubeSupport_EndPlate.THK * 2;
            }
        }


        // Dictionaries
        static public Dictionary<string, (double Height, double THK)> Dims = new Dictionary<string, (double Height, double THK)> 
        {
            //  shape       height      THK
            {   "C2x2.57", ( 2     ,   _(0.1875)   )   },
            {   "C3x4.1",  ( 3     ,   _(0.17  )   )   },
            {   "S3x5.7",  ( 3     ,   _(0.17  )   )   },
            {   "S4x7.7",  ( 4     ,   _(0.193 )   )   },
            {   "S5x10",   ( 5     ,   _(0.21  )   )   },
            {   "S6x12.5", ( 6     ,   _(0.232 )   )   },
            {   "W4x13",   ( 4.16  ,   _(0.28  )   )   },
            {   "W6x12",   ( 6.03  ,   _(0.23  )   )   },
            {   "W6x16",   ( 6.28  ,   _(0.26  )   )   },
            {   "W8x15",   ( 8.11  ,   _(0.245 )   )   },
            {   "W8x18",   ( 8.14  ,   _(0.23  )   )   },
            {   "W10x26",  (10.33  ,   _(0.26  )   )   },
        };


        // Constructor
        public TubeSupportPart(SubAssembly parentSubAssembly) : base(parentSubAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditStructuralShape(Size);
            EditDimension("Length", "sk:Path", Length);
        }


        // Private methods
        private void EditStructuralShape(string newSize)
        {
            IStructuralMemberFeatureData member = null;
            Feature feature = ModelDoc2.FirstFeature();
            string featureName = null;

            while (feature != null)
            {
                member = feature.GetDefinition() as IStructuralMemberFeatureData;
                featureName = feature.Name;

                // When found...
                if (member != null)
                {
                    string previousSize = member.ConfigurationName;
                    if (previousSize != newSize)
                    {
                        // Attempt to change size
                        ActivateModelDoc(ModelDoc2);
                        member.AccessSelections(ModelDoc2, null);
                        member.ConfigurationName = newSize;
                        feature.ModifyDefinition(member, ModelDoc2, null);
                    }

                    if (member.ConfigurationName == newSize)
                    {
                        UnsuppressFeatures(featureName);

                        // Turn off sketch
                        double number = 0;
                        int i = 0;
                        string sketchName;
                        bool isSelected = false;

                        while (!isSelected)
                        {
                            number++;
                            sketchName = "Sketch" + number;
                            isSelected = ModelDoc2.Extension.SelectByID2(sketchName, "SKETCH", 0, 0, 0, false, 0, null, 0);
                            ModelDoc2.BlankSketch();

                            if (i == 3)
                                break;
                            else if (isSelected)
                            {
                                i++;
                                isSelected = false;
                            }
                        }
                        
                    }
                    else
                    {
                        SuppressFeatures(featureName);
                    }

                }
                feature = feature.GetNextFeature();
            }

            // Special case
            featureName = "C2x2.57";
            if (Size == featureName)
                UnsuppressFeatures(featureName);
            else
                SuppressFeatures(featureName);
        }
        private static double _(double originalValue)
        {
            return Math.Ceiling(originalValue * 16) / 16;
        }


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "1560P";
        public override Shape RawMaterialShape
        {
            get
            {
                if (Size.StartsWith("C"))
                    return Part.Shape.Channel;
                else if (Size.StartsWith("S") || Size.StartsWith("W"))
                    return Part.Shape.Beam;
                else throw new NotImplementedException();
            }
        }
        public override string SizeOrThickness => Size;
        public override List<PositionData> Position
        {
            get
            {
                double yTranslation = Size.StartsWith("C") ? -Height : -Height / 2;
                double zTranslation = Size.StartsWith("C") ? THK / 2 : 0;

                return new List<PositionData>
                {
                    PositionData.Create(tY: yTranslation, tZ: zTranslation),
                };
            }
        }


        // Wrapper properties
        static public string Size
        {
            get => TubeSupportSize;
            set => TubeSupportSize = value;
        }
    }
}

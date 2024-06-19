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
        static public double ShapeHeight
        {
            get
            {
                int startIndex = 1;
                int endIndex = Size.IndexOf('x');

                string heightStr = Size.Substring(startIndex, endIndex - startIndex);
                double height = double.Parse(heightStr);

                return height;
            }
        }
        static public double Length
        {
            get
            {
                double length = Bundle.Width - SideFramePart.THK * 2;
                if (IsSmithco)
                    return length -= GetBendRadius(SideFramePart.THK) * 2;
                else
                    return length -= EndPlate.THK * 2;
            }
        }


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


        // Property overrides
        public override bool Enabled => true;
        public override string StaticPartNo => "1560P";
        public override Shape RawMaterialShape
        {
            get
            {
                if (Size.StartsWith("C"))
                    return Shape.Channel;
                else if (Size.StartsWith("S") || Size.StartsWith("W"))
                    return Shape.Beam;
                else throw new NotImplementedException();
            }
        }
        public override string SizeOrThickness => Size;
        public override List<PositionData> Position
        {
            get
            {
                double yTranslation = Size.StartsWith("C") ? -ShapeHeight : -ShapeHeight / 2;

                return new List<PositionData>
                {
                    PositionData.Create(tY: yTranslation),
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

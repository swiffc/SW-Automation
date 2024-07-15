using FileTools.Base;
using System.Collections.Generic;
using static FileTools.CommonData.CommonData;
using static HDR.HeaderBase;

namespace HDR.Connections.Derived
{
    internal abstract class Transition : Extension
    {
        // Constructor
        protected Transition(SW_Assembly parentMainAssembly) : base(parentMainAssembly) { }


        // Method overrides
        protected override void Dimensions()
        {
            EditDimension("Length", "sk:Ref", Length);
            EditDimension("Straight", "sk:Ref", Ext.Wall * 0.75);

            EditDimension("OD", "sk:Top", Ext.OD);

            EditDimension("Wall", "sk:Bottom", Ext.Wall);
            EditDimension("C", "sk:Bottom", DimC);
            EditDimension("D", "sk:Bottom", DimD);

            EditDimension("Wall", "Trans", Ext.Wall);
            EditDimension("Amount", "Bevel", Ext.Wall - Landing);
        }


        // Property overrides
        public override bool Enabled =>
            ((Location == "TL" && Header == Header61) ||
            (Location == "TR" && Header == Header62) ||
            (Location == "BL" && Header == LowestLeftHeader) ||
            (Location == "BR" && Header == LowestRightHeader)) &&
            (Ext.ExtensionType == "Trans");
        public override Shape RawMaterialShape => Shape.None;
        public override string StaticPartNo => "Trans";


        // Private methods
        double DimC
        {
            get
            {
                if (StandardSizes.ContainsKey(Ext.OD))
                    return StandardSizes[Ext.OD].C;
                else
                    return Header.BoxWidth;
            }
        }
        double DimD
        {
            get
            {
                if (StandardSizes.ContainsKey(Ext.OD))
                    return StandardSizes[Ext.OD].D;
                else
                    return Ext.OD * 1.25;
            }
        }


        // Private properties
        double Landing => 0.0625;


        // Private fields
        Dictionary<double, (double C, double D)> StandardSizes = new Dictionary<double, (double C, double D)>
        {
            {6.625, ( 4, 7.5) },
            {8.625, ( 4, 12) },
            {10.75, ( 6, 13) },
            {12.75, ( 6, 18) },
        };
    }
}

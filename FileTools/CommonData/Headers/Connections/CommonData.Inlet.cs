using FileTools.CommonData.Headers.Connections;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static FileTools.Properties.Settings;

namespace FileTools.CommonData
{
    public static partial class CommonData
    {
        public static InletNozzle Inlet = new InletNozzle();
        public class InletNozzle : IConnection
        {
            public string Location
            {
                get => Default.Location_Inlet;
                set => Default.Location_Inlet = value;
            }
            public double FlangeO
            {
                get => Default.O_Inlet;
                set => Default.O_Inlet = value;
            }
            public double FlangeQ
            {
                get => Default.Q_Inlet;
                set => Default.Q_Inlet = value;
            }
            public double FlangeR
            {
                get => Default.R_Inlet;
                set => Default.R_Inlet = value;
            }
            public double FlangeX
            {
                get => Default.X_Inlet;
                set => Default.X_Inlet = value;
            }
            public double FlangeRD
            {
                get => Default.RD_Inlet;
                set => Default.RD_Inlet = value;
            }
            public double FlangeNB
            {
                get => Default.NB_Inlet;
                set => Default.NB_Inlet = value;
            }
            public double FlangeDB
            {
                get => Default.DB_Inlet;
                set => Default.DB_Inlet = value;
            }
            public double FlangeBC
            {
                get => Default.BC_Inlet;
                set => Default.BC_Inlet = value;
            }
            public double FlangeYY
            {
                get => Default.YY_Inlet;
                set => Default.YY_Inlet = value;
            }
            public double OD
            {
                get => Default.OD_Inlet;
                set => Default.OD_Inlet = value;
            }
            public double Wall
            {
                get => Default.Wall_Inlet;
                set => Default.Wall_Inlet = value;
            }

            public double Count
            {
                get => Default.Count_Inlet;
                set => Default.Count_Inlet = value;
            }
            public double Spacing
            {
                get => Default.Spacing_Inlet;
                set => Default.Spacing_Inlet = value;
            }
            public double OffsetX
            {
                get => Default.OffsetX_Inlet;
                set => Default.OffsetX_Inlet = value;
            }
            public double ProjectionY
            {
                get => Default.ExtensionY_Inlet;
                set => Default.ExtensionY_Inlet = value;
            }

            public string ExtensionType
            {
                get => Default.ExtensionType_Inlet;
                set => Default.ExtensionType_Inlet = value;
            }
            public string FlangePartNo
            {
                get => Default.FlangePartNo_Inlet;
                set => Default.FlangePartNo_Inlet = value;
            }
            public string ExtensionPartNo
            {
                get => Default.ExtensionPartNo_Inlet;
                set => Default.ExtensionPartNo_Inlet = value;
            }
        }
    }
}

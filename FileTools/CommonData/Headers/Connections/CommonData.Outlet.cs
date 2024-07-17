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
        public static OutletNozzle Outlet = new OutletNozzle();
        public class OutletNozzle : IConnection
        {
            public string Location
            {
                get => Default.Location_Outlet;
                set => Default.Location_Outlet = value;
            }
            public double FlangeO
            {
                get => Default.O_Outlet;
                set => Default.O_Outlet = value;
            }
            public double FlangeQ
            {
                get => Default.Q_Outlet;
                set => Default.Q_Outlet = value;
            }
            public double FlangeR
            {
                get => Default.R_Outlet;
                set => Default.R_Outlet = value;
            }
            public double FlangeX
            {
                get => Default.X_Outlet;
                set => Default.X_Outlet = value;
            }
            public double FlangeRD
            {
                get => Default.RD_Outlet;
                set => Default.RD_Outlet = value;
            }
            public double FlangeNB
            {
                get => Default.NB_Outlet;
                set => Default.NB_Outlet = value;
            }
            public double FlangeDB
            {
                get => Default.DB_Outlet;
                set => Default.DB_Outlet = value;
            }
            public double FlangeBC
            {
                get => Default.BC_Outlet;
                set => Default.BC_Outlet = value;
            }
            public double FlangeYY
            {
                get => Default.YY_Outlet;
                set => Default.YY_Outlet = value;
            }
            public double OD
            {
                get => Default.OD_Outlet;
                set => Default.OD_Outlet = value;
            }
            public double Wall
            {
                get => Default.Wall_Outlet;
                set => Default.Wall_Outlet = value;
            }

            public double Count
            {
                get => Default.Count_Outlet;
                set => Default.Count_Outlet = value;
            }
            public double Spacing
            {
                get => Default.Spacing_Outlet;
                set => Default.Spacing_Outlet = value;
            }
            public double OffsetX
            {
                get => Default.OffsetX_Outlet;
                set => Default.OffsetX_Outlet = value;
            }
            public double ProjectionY
            {
                get => Default.ExtensionY_Outlet;
                set => Default.ExtensionY_Outlet = value;
            }

            public string ExtensionType
            {
                get => Default.ExtensionType_Outlet;
                set => Default.ExtensionType_Outlet = value;
            }
            public string FlangePartNo
            {
                get => Default.FlangePartNo_Outlet;
                set => Default.FlangePartNo_Outlet = value;
            }
            public string ExtensionPartNo
            {
                get => Default.ExtensionPartNo_Outlet;
                set => Default.ExtensionPartNo_Outlet = value;
            }
        }
    }
}

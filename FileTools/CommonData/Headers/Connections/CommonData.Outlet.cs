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
        public static Outlet OutletFlange = new Outlet();
        public class Outlet : IConnection
        {
            public string Location
            {
                get => Default.Location_Outlet;
                set => Default.Location_Outlet = value;
            }
            public double O
            {
                get => Default.O_Outlet;
                set => Default.O_Outlet = value;
            }
            public double Q
            {
                get => Default.Q_Outlet;
                set => Default.Q_Outlet = value;
            }
            public double R
            {
                get => Default.R_Outlet;
                set => Default.R_Outlet = value;
            }
            public double X
            {
                get => Default.X_Outlet;
                set => Default.X_Outlet = value;
            }
            public double RD
            {
                get => Default.RD_Outlet;
                set => Default.RD_Outlet = value;
            }
            public double NB
            {
                get => Default.NB_Outlet;
                set => Default.NB_Outlet = value;
            }
            public double DB
            {
                get => Default.DB_Outlet;
                set => Default.DB_Outlet = value;
            }
            public double BC
            {
                get => Default.BC_Outlet;
                set => Default.BC_Outlet = value;
            }
            public double YY
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
            public double ExtensionY
            {
                get => Default.ExtensionY_Outlet;
                set => Default.ExtensionY_Outlet = value;
            }
        }
    }
}

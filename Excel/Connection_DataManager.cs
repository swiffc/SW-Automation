using FileTools.Base;
using FileTools.CommonData.Headers.Connections;
using Microsoft.Office.Interop.Excel;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using static Excel.StaticHelpers;
using TextBox = System.Windows.Forms.TextBox;
using static Excel.Prego;
using static FileTools.CommonData.CommonData;

namespace Excel
{
    public class Connection_DataManager
    {
        public class Prego_DTO
        {
            public (string[] Cells, Worksheet Sheet) Location { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeO { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeQ { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeR { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeX { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeRD { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeNB { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeDB { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeBC { get; set; }
            public (string[] Cells, Worksheet Sheet) FlangeYY { get; set; }
            public (string[] Cells, Worksheet Sheet) OD { get; set; }
            public (string[] Cells, Worksheet Sheet) Wall { get; set; }
            public (string[] Cells, Worksheet Sheet) Count { get; set; }
            public (string[] Cells, Worksheet Sheet) Spacing { get; set; }
            public (string[] Cells, Worksheet Sheet) OffsetX { get; set; }
            public (string[] Cells, Worksheet Sheet) ProjectionY { get; set; }
        }

        public static Dictionary<string, UI_DTO> ConnectionAppData;
        public class UI_DTO
        {
            public IConnection Connection { get; set; }
            public ComboBox Location { get; set; }
            public TextBox FlangeOTextBox { get; set; }
            public TextBox FlangeQTextBox { get; set; }
            public TextBox FlangeRTextBox { get; set; }
            public TextBox FlangeXTextBox { get; set; }
            public TextBox FlangeRDTextBox { get; set; }
            public TextBox FlangeNBTextBox { get; set; }
            public TextBox FlangeDBTextBox { get; set; }
            public TextBox FlangeBCTextBox { get; set; }
            public TextBox FlangeYYTextBox { get; set; }
            public TextBox ODTextBox { get; set; }
            public TextBox WallTextBox { get; set; }
            public TextBox CountTextBox { get; set; }
            public TextBox SpacingTextBox { get; set; }
            public TextBox OffsetXTextBox { get; set; }
            public TextBox ProjectionYTextBox { get; set; }
            public TextBox FlangePartNoTextBox { get; set; }
            public TextBox ExtensionPartNoTextBox { get; set; }
        }

        public enum ConnectionNames
        {
            Inlet = 1,
            Outlet = 2,
            Vent = 3,
            Drain = 4,
            Temp = 5,
            Temp_RP2 = 6,
            Press = 7,
            Press_RP2 = 8,
            Con1 = 9,
            Con1_RP2 = 10,
            Con2 = 11,
            Con2_RP2 = 12,
            Con3 = 13,
            Con3_RP2 = 14,
        }
        public static void MapLocal_UI_To_DTO(Form formInstance)
        {
            ConnectionAppData = new Dictionary<string, UI_DTO>();

            string connectionName;
            for (int i = 1; i <= 2; i++)
            {
                connectionName = Enum.GetName(typeof(ConnectionNames), i);
                var uiDto = new UI_DTO
                {
                    Connection = GetConnection(connectionName),
                    Location = GetControl<ComboBox>(formInstance, "cLocation_", connectionName),
                    FlangeOTextBox = GetControl<TextBox>(formInstance, "tO_", connectionName),
                    FlangeQTextBox = GetControl<TextBox>(formInstance, "tQ_", connectionName),
                    FlangeRTextBox = GetControl<TextBox>(formInstance, "tR_", connectionName),
                    FlangeXTextBox = GetControl<TextBox>(formInstance, "tX_", connectionName),
                    FlangeRDTextBox = GetControl<TextBox>(formInstance, "tRD_", connectionName),
                    FlangeNBTextBox = GetControl<TextBox>(formInstance, "tNB_", connectionName),
                    FlangeDBTextBox = GetControl<TextBox>(formInstance, "tDB_", connectionName),
                    FlangeBCTextBox = GetControl<TextBox>(formInstance, "tBC_", connectionName),
                    FlangeYYTextBox = GetControl<TextBox>(formInstance, "tYY_", connectionName),
                    ODTextBox = GetControl<TextBox>(formInstance, "tOD_", connectionName),
                    WallTextBox = GetControl<TextBox>(formInstance, "tWall_", connectionName),
                    CountTextBox = GetControl<TextBox>(formInstance, "tCount_", connectionName),
                    SpacingTextBox = GetControl<TextBox>(formInstance, "tSpacing_", connectionName),
                    OffsetXTextBox = GetControl<TextBox>(formInstance, "tOffsetX_", connectionName),
                    ProjectionYTextBox = GetControl<TextBox>(formInstance, "tExtensionY_", connectionName),
                    FlangePartNoTextBox = GetControl<TextBox>(formInstance, "tFlangePartNo_", connectionName),
                    ExtensionPartNoTextBox = GetControl<TextBox>(formInstance, "tExtensionPartNo_", connectionName),
                };

                ConnectionAppData.Add(connectionName, uiDto);
            }
        }

        static Dictionary<string, Prego_DTO> _connectionPregoData
        {
            get
            {
                return new Dictionary<string, Prego_DTO>
                {
                    ["Inlet"] = new Prego_DTO
                    {
                        Location = (new[] { "L31" }, InputSheet),
                        FlangeO = (new[] { "AQX14" }, InputsCalcsSheet),
                        FlangeQ = (new[] { "AQX15" }, InputsCalcsSheet),
                        FlangeR = (new[] { "AQX16" }, InputsCalcsSheet),
                        FlangeX = (new[] { "AQX17" }, InputsCalcsSheet),
                        FlangeRD = (new[] { "AQX18" }, InputsCalcsSheet),
                        FlangeNB = (new[] { "AQX24" }, InputsCalcsSheet),
                        FlangeDB = (new[] { "AQX25" }, InputsCalcsSheet),
                        FlangeBC = (new[] { "AQX26" }, InputsCalcsSheet),
                        FlangeYY = (new[] { "AQX27" }, InputsCalcsSheet),
                        OD = (new[] { "CY40" }, InputsCalcsSheet),
                        Wall = (new[] { "DL14" }, InputsCalcsSheet),
                        Count = (new[] { "L32" }, InputSheet),
                        Spacing = (new[] { "DO37" }, InputsCalcsSheet),
                        OffsetX = (new[] { "DO40" }, InputsCalcsSheet),
                        ProjectionY = (new[] { "DL60" }, InputsCalcsSheet),
                    },
                    ["Outlet"] = new Prego_DTO
                    {
                        Location = (new[] { "DR6" }, InputsCalcsSheet),
                        FlangeO = (new[]  { "AQY14" }, InputsCalcsSheet),
                        FlangeQ = (new[]  { "AQY15" }, InputsCalcsSheet),
                        FlangeR = (new[]  { "AQY16" }, InputsCalcsSheet),
                        FlangeX = (new[]  { "AQY17" }, InputsCalcsSheet),
                        FlangeRD = (new[] { "AQY18" }, InputsCalcsSheet),
                        FlangeNB = (new[] { "AQY24" }, InputsCalcsSheet),
                        FlangeDB = (new[] { "AQY25" }, InputsCalcsSheet),
                        FlangeBC = (new[] { "AQY26" }, InputsCalcsSheet),
                        FlangeYY = (new[] { "AQY27" }, InputsCalcsSheet),
                        OD = (new[] { "EA40" }, InputsCalcsSheet),
                        Wall = (new[] { "EN14" }, InputsCalcsSheet),
                        Count = (new[] { "DR9" }, InputsCalcsSheet),
                        Spacing = (new[] { "EQ37" }, InputsCalcsSheet),
                        OffsetX = (new[] { "EQ40" }, InputsCalcsSheet),
                        ProjectionY = (new[] { "EN60" }, InputsCalcsSheet),
                    }
                };
            }
        }

        // Push application data to UI
        public static void LoadConnectionData_FromApp(string connectionName)
        {
            var connectionControls = ConnectionAppData[connectionName];
            connectionControls.Location.SelectedItem = connectionControls.Connection.Location;

            ToggleTextbox_OnOff(connectionControls);

            if (connectionControls.Connection.Location != "None")
                PushApplicationDataToTextbox(connectionControls);
            else
                SetTextboxToEmptyString(connectionControls);
        }


        // Push Prego data to application
        public static void LoadConnectionData_FromPrego(IConnection connection, ComboBox comboBox, UI_DTO uiDto, Prego_DTO prego)
        {
            connection.Location = LoadPregoString(comboBox, prego.Location.Sheet, prego.Location.Cells);
            if (connection.Location != "None" || connection.Location != "0" || connection.Location != "" || connection.Location != null)
            {
                var uiDtoType = typeof(UI_DTO);
                var pregoDtoType = typeof(Prego_DTO);
                foreach (var uiDtoProperty in uiDtoType.GetProperties())
                {
                    if (uiDtoProperty.PropertyType == typeof(TextBox))
                    {
                        string propertyName = uiDtoProperty.Name.Replace("TextBox", "");
                        var pregoDtoProperty = pregoDtoType.GetProperty(propertyName);
                        if (pregoDtoProperty != null)
                        {
                            var textBox = (TextBox)uiDtoProperty.GetValue(uiDto);
                            var cellsAndSheet = (ValueTuple<string[], Worksheet>)pregoDtoProperty.GetValue(prego);
                            if (textBox != null && cellsAndSheet.Item1 != null && cellsAndSheet.Item2 != null)
                            {
                                Type headerType = connection.GetType();
                                PropertyInfo headerProperty = headerType.GetProperty(propertyName);
                                double loadedValue = LoadPregoDouble(textBox, cellsAndSheet.Item2, cellsAndSheet.Item1);
                                textBox.Text = loadedValue == 0 ? "" : loadedValue.ToString();
                                headerProperty.SetValue(connection, loadedValue);
                            }
                        }
                    }
                }
            }
        }


        // Push application data to UI
        public static void ImportConnectionData_FromPrego()
        {

            var connectionNames = new[] { "Inlet", "Outlet"};
            foreach (var connectionName in connectionNames)
            {
                var connectionAppData = ConnectionAppData[connectionName];
                var connectionPregoData = _connectionPregoData[connectionName];
                LoadConnectionData_FromPrego
                (
                    connectionAppData.Connection,
                    connectionAppData.Location,
                    connectionAppData,
                    connectionPregoData
                );
            }
            SaveSettings();
        }


    }
}

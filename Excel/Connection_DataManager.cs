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
            public (string[] Cells, Worksheet Sheet) O { get; set; }
            public (string[] Cells, Worksheet Sheet) Q { get; set; }
            public (string[] Cells, Worksheet Sheet) R { get; set; }
            public (string[] Cells, Worksheet Sheet) X { get; set; }
            public (string[] Cells, Worksheet Sheet) RD { get; set; }
            public (string[] Cells, Worksheet Sheet) NB { get; set; }
            public (string[] Cells, Worksheet Sheet) DB { get; set; }
            public (string[] Cells, Worksheet Sheet) BC { get; set; }
            public (string[] Cells, Worksheet Sheet) YY { get; set; }
            public (string[] Cells, Worksheet Sheet) OD { get; set; }
            public (string[] Cells, Worksheet Sheet) Wall { get; set; }
            public (string[] Cells, Worksheet Sheet) Count { get; set; }
            public (string[] Cells, Worksheet Sheet) Spacing { get; set; }
            public (string[] Cells, Worksheet Sheet) OffsetX { get; set; }
            public (string[] Cells, Worksheet Sheet) ExtensionY { get; set; }
        }

        public static Dictionary<string, UI_DTO> ConnectionAppData;
        public class UI_DTO
        {
            public IConnection Connection { get; set; }
            public ComboBox Location { get; set; }
            public TextBox OTextBox { get; set; }
            public TextBox QTextBox { get; set; }
            public TextBox RTextBox { get; set; }
            public TextBox XTextBox { get; set; }
            public TextBox RDTextBox { get; set; }
            public TextBox NBTextBox { get; set; }
            public TextBox DBTextBox { get; set; }
            public TextBox BCTextBox { get; set; }
            public TextBox YYTextBox { get; set; }
            public TextBox ODTextBox { get; set; }
            public TextBox WallTextBox { get; set; }
            public TextBox CountTextBox { get; set; }
            public TextBox SpacingTextBox { get; set; }
            public TextBox OffsetXTextBox { get; set; }
            public TextBox ExtensionYTextBox { get; set; }
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
                    OTextBox = GetControl<TextBox>(formInstance, "tO_", connectionName),
                    QTextBox = GetControl<TextBox>(formInstance, "tQ_", connectionName),
                    RTextBox = GetControl<TextBox>(formInstance, "tR_", connectionName),
                    XTextBox = GetControl<TextBox>(formInstance, "tX_", connectionName),
                    RDTextBox = GetControl<TextBox>(formInstance, "tRD_", connectionName),
                    NBTextBox = GetControl<TextBox>(formInstance, "tNB_", connectionName),
                    DBTextBox = GetControl<TextBox>(formInstance, "tDB_", connectionName),
                    BCTextBox = GetControl<TextBox>(formInstance, "tBC_", connectionName),
                    YYTextBox = GetControl<TextBox>(formInstance, "tYY_", connectionName),
                    ODTextBox = GetControl<TextBox>(formInstance, "tOD_", connectionName),
                    WallTextBox = GetControl<TextBox>(formInstance, "tWall_", connectionName),
                    CountTextBox = GetControl<TextBox>(formInstance, "tCount_", connectionName),
                    SpacingTextBox = GetControl<TextBox>(formInstance, "tSpacing_", connectionName),
                    OffsetXTextBox = GetControl<TextBox>(formInstance, "tOffsetX_", connectionName),
                    ExtensionYTextBox = GetControl<TextBox>(formInstance, "tExtensionY_", connectionName),
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
                        O = (new[] { "AQX14" }, InputsCalcsSheet),
                        Q = (new[] { "AQX15" }, InputsCalcsSheet),
                        R = (new[] { "AQX16" }, InputsCalcsSheet),
                        X = (new[] { "AQX17" }, InputsCalcsSheet),
                        RD = (new[] { "AQX18" }, InputsCalcsSheet),
                        NB = (new[] { "AQX24" }, InputsCalcsSheet),
                        DB = (new[] { "AQX25" }, InputsCalcsSheet),
                        BC = (new[] { "AQX26" }, InputsCalcsSheet),
                        YY = (new[] { "AQX27" }, InputsCalcsSheet),
                        OD = (new[] { "CY40" }, InputsCalcsSheet),
                        Wall = (new[] { "DL14" }, InputsCalcsSheet),
                        Count = (new[] { "L32" }, InputSheet),
                        Spacing = (new[] { "DO37" }, InputsCalcsSheet),
                        OffsetX = (new[] { "DO40" }, InputsCalcsSheet),
                        ExtensionY = (new[] { "DL60" }, InputsCalcsSheet),
                    },
                    ["Outlet"] = new Prego_DTO
                    {
                        Location = (new[] { "DR6" }, InputsCalcsSheet),
                        O = (new[]  { "AQY14" }, InputsCalcsSheet),
                        Q = (new[]  { "AQY15" }, InputsCalcsSheet),
                        R = (new[]  { "AQY16" }, InputsCalcsSheet),
                        X = (new[]  { "AQY17" }, InputsCalcsSheet),
                        RD = (new[] { "AQY18" }, InputsCalcsSheet),
                        NB = (new[] { "AQY24" }, InputsCalcsSheet),
                        DB = (new[] { "AQY25" }, InputsCalcsSheet),
                        BC = (new[] { "AQY26" }, InputsCalcsSheet),
                        YY = (new[] { "AQY27" }, InputsCalcsSheet),
                        OD = (new[] { "EA40" }, InputsCalcsSheet),
                        Wall = (new[] { "EN14" }, InputsCalcsSheet),
                        Count = (new[] { "DR9" }, InputsCalcsSheet),
                        Spacing = (new[] { "EQ37" }, InputsCalcsSheet),
                        OffsetX = (new[] { "EQ40" }, InputsCalcsSheet),
                        ExtensionY = (new[] { "EN60" }, InputsCalcsSheet),
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

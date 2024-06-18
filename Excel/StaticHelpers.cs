using Microsoft.Office.Interop.Excel;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using static Excel.Header_DataManager;
using System.Windows.Forms;
using static Excel.Prego;
using TextBox = System.Windows.Forms.TextBox;
using CheckBox = System.Windows.Forms.CheckBox;

namespace Excel
{
    public class StaticHelpers
    {
        public static double LoadPregoDouble(TextBox textBox, Worksheet worksheet, double valueIfSmithco, double ValueIfHPC)
        {
            string[] cellNames = new string[]
            {
                "G" + 27, // Override
                "F" + 27 // Titleblock Manuf
            };

            string titleblockManuf = CellString(worksheet, cellNames);

            if (titleblockManuf == "Smithco")
            {
                textBox.Text = valueIfSmithco.ToString();
                return valueIfSmithco;
            }
            else
            {
                textBox.Text = ValueIfHPC.ToString();
                return ValueIfHPC;
            }
        }
        public static double LoadPregoDouble_FeetToInches(TextBox textBox, Worksheet worksheet, params string[] cellNames)
        {
            string tubeLength_ArchitecturalFeet = CellString(worksheet, cellNames[0]);
            double tubeLength_DecimalFeet = CellDouble(worksheet, cellNames[1]);

            if (tubeLength_ArchitecturalFeet != null)
            {
                // #'-#" --> decimal inches
                var parts = tubeLength_ArchitecturalFeet.Split('\'', '\"');
                double feet = double.Parse(parts[0]);
                double inches = double.Parse(parts[1]);
                double decimalInches = feet * 12 + inches;

                textBox.Text = decimalInches.ToString();
                return decimalInches;
            }
            else // Decimal feet --> decimal inches
            {
                double decimalInches = tubeLength_DecimalFeet * 12;
                textBox.Text = decimalInches.ToString();
                return decimalInches;
            }
        }
        public static double LoadPregoDouble(TextBox textBox, Worksheet worksheet, params string[] cellNames)
        {
            double value = CellDouble(worksheet, cellNames);
            textBox.Text = value.ToString();
            return value;
        }
        public static int LoadPregoInt(TextBox textBox, Worksheet worksheet, params string[] cellNames)
        {
            int value = (int)CellDouble(worksheet, cellNames);
            textBox.Text = value.ToString();
            return value;
        }
        public static T LoadPregoValue<T>(Control control, Worksheet worksheet, params string[] cellNames)
            where T : IConvertible
        {
            var value = CellString(worksheet, cellNames);
            if (value == null)
            {
                value = CellDouble(worksheet, cellNames).ToString();
            }
            control.Text = value;
            return (T)Convert.ChangeType(value, typeof(T));
        }
        public static bool LoadPregoBool_NullOrEmpty(CheckBox checkBox, Worksheet worksheet, params string[] cellNames)
        {
            string value = CellString(worksheet, cellNames);
            bool enabled = value == null ? false : true;
            checkBox.Checked = enabled;
            return enabled;
        }
        public static bool LoadPregoBool(CheckBox checkBox, Worksheet worksheet, params string[] cellNames)
        {
            string value = CellString(worksheet, cellNames).ToLower();
            bool enabled;
            if (value == "yes" || value == "true")
            {
                enabled = true;
            }
            else if (value == "no" || value == "false")
            {
                enabled = false;
            }
            else
            {
                throw new FormatException("The cell does not contain a recognized boolean value.");
            }
            checkBox.Checked = enabled;
            return enabled;
        }
        public static void ToggleTextbox_OnOff(UI_DTO headerControls)
        {
            foreach (var property in headerControls.GetType().GetProperties())
            {
                if (property.PropertyType == typeof(TextBox))
                {
                    TextBox textBox = (TextBox)property.GetValue(headerControls);
                    if (textBox != null)
                        textBox.Enabled = headerControls.Header.IsRequired;
                }
            }
        }
        public static void PushApplicationDataToTextbox(UI_DTO headerControls)
        {
            foreach (var property in headerControls.Header.GetType().GetProperties())
            {
                if (property.PropertyType == typeof(double))
                {
                    double value = (double)property.GetValue(headerControls.Header);
                    Type headerControlsType = headerControls.GetType();

                    string propertyName = property.Name + "TextBox";
                    PropertyInfo propertyInfo = headerControlsType.GetProperty(propertyName);

                    object propertyValue = propertyInfo.GetValue(headerControls);
                    TextBox textBox = (TextBox)propertyValue;

                    textBox.Text = value.ToString();
                }
            }
        }
        public static void SetTextboxToEmptyString(UI_DTO headerControls)
        {
            foreach (var property in headerControls.GetType().GetProperties())
            {
                if (property.PropertyType == typeof(TextBox))
                {
                    TextBox textBox = (TextBox)property.GetValue(headerControls);
                    if (textBox != null)
                        textBox.Text = "";
                }
            }
        }
    }
}

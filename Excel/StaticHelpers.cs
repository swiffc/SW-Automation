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
using FileTools.Base;
using static FileTools.CommonData.CommonData;

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

                    if (propertyInfo != null)
                    {
                        TextBox textBox = (TextBox)propertyInfo.GetValue(headerControls);
                        if (textBox != null)
                            textBox.Text = value.ToString();
                    }
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
        public static IHeaderExtensions GetHeader(int index)
        {
            switch (index)
            {
                case 61:
                    return Header61;
                case 62:
                    return Header62;
                case 63:
                    return Header63;
                case 64:
                    return Header64;
                case 65:
                    return Header65;
                case 66:
                    return Header66;
                default:
                    throw new ArgumentException($"Invalid index: {index}");
            }
        }
        public static T GetControl<T>(Form formInstance, string baseName, int index) where T : class
        {
            var controlName = $"{baseName}{index}";
            return formInstance.GetType().GetField(controlName, BindingFlags.Instance | BindingFlags.NonPublic)?.GetValue(formInstance) as T;
        }
        public static void HeaderTextBoxDoubleChanged(object sender, EventArgs e)
        {
            if (sender is TextBox textBox)
            {
                var parts = textBox.Tag.ToString().Split(':');
                if (parts.Length == 2)
                {
                    int headerId = int.Parse(parts[0]);
                    string propertyName = parts[1];
                    if (textBox.Text != "")
                    {
                        double value = double.TryParse(textBox.Text, out double parsedValue) ? parsedValue : 0;

                        var header = GetHeader(headerId);
                        if (header != null)
                        {
                            var property = header.GetType().GetProperty(propertyName);
                            if (property != null && property.PropertyType == typeof(double))
                            {
                                property.SetValue(header, value);
                                SaveSettings();
                            }
                        }
                    }
                }
            }
        }
        public static void Header_TextChanged(Form formInstance, string propertyName)
        {
            foreach (Control foundTabControl in formInstance.Controls)
            {
                if (foundTabControl is TabControl tabControl)
                {
                    foreach (Control foundTabPage in tabControl.Controls)
                    {
                        if (foundTabPage is TabPage tabPage)
                        {
                            foreach (Control foundPanel in tabPage.Controls)
                            {
                                if (foundPanel is Panel panel)
                                {
                                    foreach (Control control in panel.Controls)
                                    {
                                        if (control is TextBox textBox && textBox.Name.StartsWith("t" + propertyName))
                                        {
                                            // Extract the suffix from the TextBox name by removing the prefix that matches the propertyName
                                            string suffix = textBox.Name.Substring(("t" + propertyName).Length);

                                            // Split the suffix into parts based on the '_' delimiter
                                            string[] parts = suffix.Split('_');

                                            // Ensure that the suffix correctly splits into exactly two parts: propertyType and id
                                            if (parts.Length == 2)
                                            {
                                                // The first part represents the property type (e.g., "THK" for thickness)
                                                string propertyType = parts[0];
                                                // The second part represents the unique identifier (e.g., "61")
                                                string id = parts[1];

                                                // Construct the Tag property for the TextBox using the id and propertyType
                                                // This Tag is later used to identify the TextBox and map it to a specific property of a Header object
                                                textBox.Tag = $"{id}:{propertyName}{propertyType}";

                                                // Attach the HeaderTextBoxDoubleChanged event handler to the TextBox
                                                // This event handler will be triggered whenever the text in the TextBox changes
                                                textBox.TextChanged += HeaderTextBoxDoubleChanged;
                                            }
                                            else throw new ArgumentException("Invalid TextBox name format");
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

    }
}

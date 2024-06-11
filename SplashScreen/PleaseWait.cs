using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace SplashScreen
{
    public static class PleaseWait
    {

        public static void Start(string message)
        {
            var thread = new Thread(() =>
            {
                _messageBoxForm = new MessageBoxForm(message);
                Application.Run(_messageBoxForm);
            });
            thread.SetApartmentState(ApartmentState.STA);
            thread.Start();
        }


        public static void Hide()
        {
            if (_messageBoxForm != null)
            {
                _messageBoxForm.Invoke(new Action(() => _messageBoxForm.Hide()));
            }
        }


        public static void Show(string newMessage)
        {
            if (_messageBoxForm != null)
            {
                _messageBoxForm.Invoke(new Action(() =>
                {
                    _messageBoxForm.UpdateMessage(newMessage);
                    _messageBoxForm.Show();
                }));
            }
        }


        public static void Stop()
        {
            _messageBoxForm.Invoke(new Action(() => _messageBoxForm.Close()));
        }


        private static MessageBoxForm _messageBoxForm;
        private class MessageBoxForm : Form
        {
            private Label _messageLabel;

            public MessageBoxForm(string message)
            {
                // Set the form's StartPosition property to center the form
                StartPosition = FormStartPosition.CenterScreen;

                // Set the form's FormBorderStyle property to FixedDialog
                FormBorderStyle = FormBorderStyle.FixedDialog;

                // Set the form's ControlBox property to false
                ControlBox = false;

                _messageLabel = new Label
                {
                    Text = message,
                    AutoSize = true,
                    Font = new Font("Arial", 24F, FontStyle.Regular, GraphicsUnit.Point, ((byte)(0))), // Set the font size to 14
                    Dock = DockStyle.Fill, // Make the label fill the form
                };

                Controls.Add(_messageLabel);

                // Set the form's AutoSize property to true and set the AutoSizeMode to GrowAndShrink
                AutoSize = true;
                AutoSizeMode = AutoSizeMode.GrowAndShrink;
            }

            public void UpdateMessage(string newMessage)
            {
                _messageLabel.Text = newMessage;
            }
        }
    }
}

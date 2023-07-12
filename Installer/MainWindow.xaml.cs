using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using Installation_Tools;

namespace Installer
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            InstallationTools.GetLatestInstaller();
        }

        private void Install_Click(object sender, RoutedEventArgs e)
        {
            InstallationTools.KillSolidworks();
            InstallationTools.GetLatestAddin();
            InstallationTools.Install();
        }

        private void Uninstall_Click(object sender, RoutedEventArgs e)
        {
            InstallationTools.KillSolidworks();
            InstallationTools.Uninstall();
        }

        private void RepairUpdate_Click(object sender, RoutedEventArgs e)
        {
            InstallationTools.KillSolidworks();
            InstallationTools.SilentUninstall();
            InstallationTools.GetLatestAddin();
            InstallationTools.Install();

        }
    }
}

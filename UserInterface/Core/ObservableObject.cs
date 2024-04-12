using System.ComponentModel; 
using System.Runtime.CompilerServices; 

namespace UserInterface.Core
{
    class ObservableObject : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        // Method to call the PropertyChanged event handler
        // This method uses the CallerMemberName attribute to avoid having to specify the property name as a string
        // It defaults to the name of the method/property that called it
        protected virtual void OnPropertyChanged([CallerMemberName] string name = null)
        {
            // ?. is the null-conditional operator: it only invokes the method if PropertyChanged is not null
            // This prevents a NullReferenceException if there are no subscribers to the event
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(name));
        }
    }
}

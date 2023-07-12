namespace SolidWorks_Add_In
{
    partial class TaskpaneHostUI
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.Align_Dimensions = new System.Windows.Forms.Button();
            this.Delete_Dangling_Annotations = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // Align_Dimensions
            // 
            this.Align_Dimensions.Location = new System.Drawing.Point(3, 3);
            this.Align_Dimensions.Name = "Align_Dimensions";
            this.Align_Dimensions.Size = new System.Drawing.Size(193, 33);
            this.Align_Dimensions.TabIndex = 0;
            this.Align_Dimensions.Text = "Align Dimensions";
            this.Align_Dimensions.UseVisualStyleBackColor = true;
            this.Align_Dimensions.Click += new System.EventHandler(this.AlignDimensions_Click);
            // 
            // Delete_Dangling_Annotations
            // 
            this.Delete_Dangling_Annotations.Location = new System.Drawing.Point(3, 42);
            this.Delete_Dangling_Annotations.Name = "Delete_Dangling_Annotations";
            this.Delete_Dangling_Annotations.Size = new System.Drawing.Size(193, 33);
            this.Delete_Dangling_Annotations.TabIndex = 1;
            this.Delete_Dangling_Annotations.Text = "Delete Dangling Annotations";
            this.Delete_Dangling_Annotations.UseVisualStyleBackColor = true;
            this.Delete_Dangling_Annotations.Click += new System.EventHandler(this.DeleteDanglingDimensions_Click);
            // 
            // TaskpaneHostUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.Delete_Dangling_Annotations);
            this.Controls.Add(this.Align_Dimensions);
            this.Margin = new System.Windows.Forms.Padding(2);
            this.Name = "TaskpaneHostUI";
            this.Size = new System.Drawing.Size(199, 128);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button Align_Dimensions;
        private System.Windows.Forms.Button Delete_Dangling_Annotations;
    }
}

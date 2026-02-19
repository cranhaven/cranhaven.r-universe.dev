### Loading the Annotation Lookup CSV File

Welcome to the help page for loading the Annotation Lookup CSV file! This guide will help you prepare and load a CSV file to populate the dropdown box for annotations created in the annotations panel.

#### About the Annotation Lookup CSV File

The Annotation Lookup CSV file is used to populate the dropdown menu in the annotations panel with options that users can select when creating annotations. This file should have two columns: 1. **display**: Contains the text shown in the dropdown menu. 2. **value**: Contains the corresponding data to be written in the annotations dataframe.

#### Requirements for the CSV File

-   **Two Columns**: The CSV must include exactly two columns with headers:
    -   `display`: This is the text that will appear in the dropdown menu for the user to select.
    -   `value`: This is the data that will be stored in the annotations dataframe.
-   **Content**:
    -   Both columns can contain any text. However, it is a good idea to consider how you might want to use the annotation data later. For example, if you plan to plot data, it may be useful to store numbers instead of text in the `value` column.
-   **File Encoding**: The file should be encoded in UTF-8 to ensure compatibility.

#### Steps to Prepare Your CSV File

1.  **Create a Spreadsheet**: Start with a spreadsheet program like Microsoft Excel or Google Sheets.
2.  **Enter Data**: In the first column, input the display text under the header `display`. In the second column, input the corresponding values under the header `value`.
3.  **Consider Future Use**: Think about how you might use the annotation data in the future. If plotting or analysis is planned, you may want to store numeric values or codes in the `value` column.
4.  **Save as CSV**: Export the spreadsheet as a CSV file. Make sure to select UTF-8 as the encoding format if given the option.

#### Steps to Use the Browse Button

1.  **Click the Browse Button**: Click the browse button on the interface to open a file dialog window.
2.  **Select Your CSV File**: Navigate to the location of your prepared CSV file on your computer, select it, and click 'Open'.
3.  **Loading the File**: Once the file is selected, the application will load the display texts and corresponding values. The display texts will appear in the dropdown menu, and the values will be stored in the annotations dataframe when selected.

#### Troubleshooting

-   **File Not Loading**: If the CSV file fails to load, ensure it is correctly formatted with the proper headers and encoding.
-   **Incorrect Display in Dropdown**: If the display texts do not appear correctly in the dropdown, check for encoding issues or incorrect headers in the CSV file.

We hope this guide helps you effectively prepare and load your Annotation Lookup CSV file. For further assistance, please contact our support team.

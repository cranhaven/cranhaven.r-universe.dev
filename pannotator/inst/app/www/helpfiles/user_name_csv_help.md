### Loading the Username Lookup CSV File

Welcome to the help page for loading the Username Lookup CSV file! This guide will assist you in preparing and loading a CSV file with specific requirements for user name lookup functionality.

#### About the Username Lookup CSV File

The Username Lookup CSV file is used to setuip the usernames that you would like to use for image annotations. This file should have two columns: 1. **user_name**: Displayed in the dropdown menu for user selection. 2. **value**: Used to create filenames for annotation files.

#### Requirements for the CSV File

-   **Two Columns**: The CSV must include exactly two columns with headers:
    -   `user_name`: Contains the usernames as they should appear in the dropdown.
    -   `value`: Contains a corresponding filename-safe string for each username.
-   **Formatting Rules for the 'value' Column**:
    -   No spaces.
    -   No special characters that are not allowed in file names (e.g., no  / : \* ? " \< \> \|).
-   **File Encoding**: The file should be encoded in UTF-8 to ensure compatibility.

#### Steps to Prepare Your CSV File

1.  **Create a Spreadsheet**: Start with a spreadsheet program like Microsoft Excel or Google Sheets.
2.  **Enter Data**: In the first column, input the usernames under the header `user_name`. In the second column, input the corresponding filename-safe values under the header `value`.
3.  **Check Formatting**: Ensure that the values in the 'value' column do not contain any spaces or invalid characters.
4.  **Save as CSV**: Export the spreadsheet as a CSV file. Make sure to select UTF-8 as the encoding format if given the option.

#### Steps to Use the Browse Button

1.  **Click the Browse Button**: Click the browse button on the interface to open a file dialog window.
2.  **Select Your CSV File**: Navigate to the location of your prepared CSV file on your computer, select it, and click 'Open'.
3.  **Loading the File**: Once the file is selected, the application will load the usernames and corresponding values. The usernames will appear in the dropdown menu, and the values will be used for creating annotation files.

#### Troubleshooting

-   **File Not Loading**: If the CSV file fails to load, ensure it is correctly formatted with the proper headers and encoding.
-   **Incorrect Display in Dropdown**: If usernames do not display correctly in the dropdown, check for encoding issues or incorrect headers in the CSV file.

We hope this guide helps you effectively prepare and load your Username Lookup CSV file.

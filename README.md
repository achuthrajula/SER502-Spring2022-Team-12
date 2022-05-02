# SER 502: Team 12

## Required installations

| Installation      | Version       |
| ----------------- | ------------- |
| python            | 3.8.10        |
| swipl             | 8.4.2 (MacOS) |
| MacOS (Optional)  | 12.3.1        |
| Ubuntu (Optional) | 20.4.04       |

## Execution

- Make sure you have the right version of the required installations by running the following command:
  ```
  python --version && swipl --version
  ```
- Clone the git repository and navigate to the root directory and run the following command:
  ```
  python install -e .
  ```
  This installs the python module locally
- To invoke the module, run the following command:
  ```
  venom
  ```
- The program will run and detect all the executable files in the /venom/data directory. A few sample executions are provided and incase you need to run your own executable files, you need to place it in the /venom/data directory and ensure that the executable has `.vnm` extension.

- You need to select the index of the file you want to execute.

- The program then executes the file and displays the output.

Languages used: Python and Prolog

Note: The `/src`, `/data` are placed inside `/venom` and `docs` is placed in the root directory.

## Contributors:

- Achuth Rajula
- Varshik Sonti
- Rahul Vuppla
- Rajiv Jalakkam
- Laxman Rao

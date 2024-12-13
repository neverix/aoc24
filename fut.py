import subprocess

def run_subprocess_with_input(command, input_string):
    try:
        # Run the subprocess with input, capture output and error
        result = subprocess.run(
            command, 
            input=input_string,#.encode('utf-8'),  # Convert input to bytes
            capture_output=True,  # Captures both stdout and stderr
            text=True,  # Returns output as strings instead of bytes
            check=True  # Raises CalledProcessError if the command returns a non-zero exit code
        )
        
        # Print stdout
        print("Standard Output:")
        print(result.stdout)
        
        # Print stderr (if any)
        if result.stderr:
            print("\nStandard Error:")
            print(result.stderr)
        
        return result
    
    except subprocess.CalledProcessError as e:
        # Handle errors if the subprocess fails
        print(f"Command failed with exit code {e.returncode}")
        print("Standard Output:")
        print(e.stdout)
        print("\nStandard Error:")
        print(e.stderr)
        raise


to_write = str(list(map(ord, open("input.txt").read())))
result = run_subprocess_with_input("./executable", to_write).stdout
try:
    from pprint import pprint
    print("Pretty:")
    pprint(eval(result.replace("i64", "").replace("i32", "").replace("u8", "").replace("f64", "")))
except (ValueError, TypeError):
    print("Warning:")
    import traceback
    traceback.print_exc()
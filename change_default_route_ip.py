import subprocess
from pathlib import Path
from rich import print

def run_command(command):
    result = subprocess.run(command, capture_output=True, text=True, shell=True)
    return result.stdout.strip()

# Find the device used for the default route
default_route_device = run_command("ip route show default | awk '{print $5}'")

if not default_route_device:
    print("[bold red]Error:[/bold red] No default route device found.")
    exit(1)

print(f"[bold green]Default route device:[/bold green] {default_route_device}")

# Get the IP address returned by hostname -i
new_ip = run_command("hostname -i")

if not new_ip:
    print("[bold red]Error:[/bold red] Unable to get IP address from hostname -i.")
    exit(1)

print(f"[bold green]New IP address:[/bold green] {new_ip}")

# Change the IP address of the identified device
current_ip = run_command(f"ip addr show dev {default_route_device} | awk '/inet / {{print $2}}'")

if not current_ip:
    print(f"[bold red]Error:[/bold red] Unable to get current IP address for {default_route_device}")
    exit(1)

print(f"[bold green]Current IP address:[/bold green] {current_ip}")

# Remove the current IP address
remove_ip_cmd = f"sudo ip addr del {current_ip} dev {default_route_device}"
run_command(remove_ip_cmd)

# Add the new IP address
add_ip_cmd = f"sudo ip addr add {new_ip} dev {default_route_device}"
result = run_command(add_ip_cmd)

if result:
    print(f"[bold red]Error changing IP address:[/bold red] {result}")
else:
    print(f"[bold green]Successfully changed IP address of {default_route_device} to {new_ip}[/bold green]")

# Verify the change
new_current_ip = run_command(f"ip addr show dev {default_route_device} | awk '/inet / {{print $2}}'")
print(f"[bold green]New current IP address:[/bold green] {new_current_ip}")
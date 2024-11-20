#!/usr/bin/env python3
#
# Number converter, with support for split octal (as used by Heathkit
# 8080 and Z80-based computers).
#
# Jeff Tranter <tranter@pobox.com>

import sys
import tkinter as tk
from tkinter import messagebox


# Function to convert decimal to other formats
def decimal_to_others():
    try:
        decimal = int(entry_decimal.get())

        # Convert decimal to binary, octal, split octal, and hexadecimal
        binary = bin(decimal)[2:].zfill(16)
        octal = oct(decimal)[2:].zfill(6)
        split = oct(decimal >> 8)[2:].zfill(3) + "." + oct(decimal % 256)[2:].zfill(3)
        hexadecimal = hex(decimal)[2:].upper().zfill(4)

        # Update the other fields with the converted values
        entry_binary.delete(0, tk.END)
        entry_binary.insert(0, binary)

        entry_octal.delete(0, tk.END)
        entry_octal.insert(0, octal)

        entry_split.delete(0, tk.END)
        entry_split.insert(0, split)

        entry_hex.delete(0, tk.END)
        entry_hex.insert(0, hexadecimal)
    except ValueError:
        messagebox.showerror("Invalid Input", "Please enter a valid decimal number.")


# Function to convert binary to other formats
def binary_to_others():
    try:
        binary = entry_binary.get()
        decimal = int(binary, 2)

        octal = oct(decimal)[2:].zfill(6)
        split = oct(decimal >> 8)[2:].zfill(3) + "." + oct(decimal % 256)[2:].zfill(3)
        hexadecimal = hex(decimal)[2:].upper().zfill(4)

        entry_decimal.delete(0, tk.END)
        entry_decimal.insert(0, str(decimal))

        entry_octal.delete(0, tk.END)
        entry_octal.insert(0, octal)

        entry_split.delete(0, tk.END)
        entry_split.insert(0, split)

        entry_hex.delete(0, tk.END)
        entry_hex.insert(0, hexadecimal)
    except ValueError:
        messagebox.showerror("Invalid Input", "Please enter a valid binary number.")


# Function to convert octal to other formats
def octal_to_others():
    try:
        octal = entry_octal.get().zfill(6)
        decimal = int(octal, 8)
        split = oct(decimal >> 8)[2:].zfill(3) + "." + oct(decimal % 256)[2:].zfill(3)

        binary = bin(decimal)[2:].zfill(16)
        hexadecimal = hex(decimal)[2:].upper().zfill(4)

        entry_decimal.delete(0, tk.END)
        entry_decimal.insert(0, str(decimal))

        entry_binary.delete(0, tk.END)
        entry_binary.insert(0, binary)

        entry_split.delete(0, tk.END)
        entry_split.insert(0, split)

        entry_hex.delete(0, tk.END)
        entry_hex.insert(0, hexadecimal)
    except ValueError:
        messagebox.showerror("Invalid Input", "Please enter a valid octal number.")


# Function to convert split octal to other formats
def split_to_others():
    try:
        parts = entry_split.get().split(".", 1)
        p1 = int(parts[0], 8)
        p2 = int(parts[1], 8)
        decimal = p1 * 256 + p2

        octal = oct(decimal)[2:].zfill(6)
        binary = bin(decimal)[2:].zfill(16)
        hexadecimal = hex(decimal)[2:].upper().zfill(4)

        entry_decimal.delete(0, tk.END)
        entry_decimal.insert(0, str(decimal))

        entry_binary.delete(0, tk.END)
        entry_binary.insert(0, binary)

        entry_octal.delete(0, tk.END)
        entry_octal.insert(0, octal)

        entry_hex.delete(0, tk.END)
        entry_hex.insert(0, hexadecimal)
    except (ValueError, IndexError):
        messagebox.showerror("Invalid Input", "Please enter a valid split octal number.")


# Function to convert hexadecimal to other formats
def hex_to_others():
    try:
        hex_value = entry_hex.get()
        decimal = int(hex_value, 16)

        binary = bin(decimal)[2:].zfill(16)
        octal = oct(decimal)[2:].zfill(6)
        split = oct(decimal >> 8)[2:].zfill(3) + "." + oct(decimal % 256)[2:].zfill(3)

        entry_decimal.delete(0, tk.END)
        entry_decimal.insert(0, str(decimal))

        entry_binary.delete(0, tk.END)
        entry_binary.insert(0, binary)

        entry_octal.delete(0, tk.END)
        entry_octal.insert(0, octal)

        entry_split.delete(0, tk.END)
        entry_split.insert(0, split)
    except ValueError:
        messagebox.showerror("Invalid Input", "Please enter a valid hexadecimal number.")


def quit():
    sys.exit()


# Create the main window
root = tk.Tk()
root.title("Number System Converter")

# Create and place the labels
label_decimal = tk.Label(root, text="Decimal:")
label_decimal.grid(row=0, column=0, padx=10, pady=10)

label_binary = tk.Label(root, text="Binary:")
label_binary.grid(row=1, column=0, padx=10, pady=10)

label_octal = tk.Label(root, text="Octal:")
label_octal.grid(row=2, column=0, padx=10, pady=10)

label_split = tk.Label(root, text="Split Octal:")
label_split.grid(row=3, column=0, padx=10, pady=10)

label_hex = tk.Label(root, text="Hexadecimal:")
label_hex.grid(row=4, column=0, padx=10, pady=10)

# Create and place the input fields
entry_decimal = tk.Entry(root)
entry_decimal.grid(row=0, column=1, padx=10, pady=10)

entry_binary = tk.Entry(root)
entry_binary.grid(row=1, column=1, padx=10, pady=10)

entry_octal = tk.Entry(root)
entry_octal.grid(row=2, column=1, padx=10, pady=10)

entry_split = tk.Entry(root)
entry_split.grid(row=3, column=1, padx=10, pady=10)

entry_hex = tk.Entry(root)
entry_hex.grid(row=4, column=1, padx=10, pady=10)

# Create and place the conversion buttons
button_convert_decimal = tk.Button(root, text="Convert", command=decimal_to_others)
button_convert_decimal.grid(row=0, column=2, padx=10, pady=10)

button_convert_binary = tk.Button(root, text="Convert", command=binary_to_others)
button_convert_binary.grid(row=1, column=2, padx=10, pady=10)

button_convert_octal = tk.Button(root, text="Convert", command=octal_to_others)
button_convert_octal.grid(row=2, column=2, padx=10, pady=10)

button_convert_split = tk.Button(root, text="Convert", command=split_to_others)
button_convert_split.grid(row=3, column=2, padx=10, pady=10)

button_convert_hex = tk.Button(root, text="Convert", command=hex_to_others)
button_convert_hex.grid(row=4, column=2, padx=10, pady=10)

button_quit = tk.Button(root, text="Quit", command=quit)
button_quit.grid(row=5, column=1, padx=10, pady=10)

# Start the Tkinter event loop
root.mainloop()

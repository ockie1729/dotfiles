#!/usr/bin/env python3
# coding: utf-8
import argparse


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--tmp", type=str, required=True)
    args = parser.parse_args()


if __name__ == "__main__":
    main()

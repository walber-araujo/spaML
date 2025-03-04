# 🚀 spaML - Spam Classifier in Haskell

spaML is a spam message classifier developed in **Haskell**, using the **Naive Bayes** algorithm to categorize messages as **spam** or **ham** (not spam).

---

## 📌 Development Team
- **Alex Silva**
- **João Brandão**
- **Vinícius Porto**
- **Walber Araújo**

---

## 🛠️ Features
✅ Model training from CSV files  
✅ Reuse of previously trained models  
✅ Manual training  
✅ Classification of individual messages  
✅ Display of accuracy metrics of trained models  
✅ Interactive interface via command line (CLI)  

---

## 📁 Project Structure

```
spaML/
│── src/ # Project source code
│── data/ # Dataset for training/testing
│── models/ # Saved trained models
│── spaML.cabal # Cabal configuration file
│── README.md # Project documentation
```

---

## 📋 Requirements
- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Cabal](https://www.haskell.org/cabal/)
- [Stack](https://docs.haskellstack.org/en/stable/README/)

---

## 🚀 How to Run

1️⃣ Clone the repository:
```sh
git clone https://github.com/walber-araujo/spaML.git && cd spaML
```

2️⃣ Build the project:
```sh
stack build
```

3️⃣ Run:

Linux/MacOS:

```sh
stack run
```

Windows:

```sh
stack run -- +RTS --io-manager=native
```

---

## 🖥️ Usage

### 📌 CLI Options:
| Option | Action |
|---------|--------|
| **1** | Train model with a CSV file |
| **2** | Reuse trained model |
| **3** | Manual training |
| **4** | Classify message |
| **5** | Show model accuracy |
| **6** | Exit |

### 📂 CSV file format
CSV files used for training must follow this format:
```
label,message
ham,Hello, how are you?
spam,You won a free prize! Click here!
```
> Where `label` can be **ham** or **spam**.

---

## 📜 License
This project is under the [MIT license](https://opensource.org/licenses/MIT).

---

💡 **Contributions are welcome!** Feel free to open **issues** and send **pull requests**. 😊

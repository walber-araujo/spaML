# 🚀 spaML  

# 🚀 spaML - Classificador de Spam em Haskell
spaML é um classificador de mensagens de spam desenvolvido em *Haskell, utilizando o algoritmo **Naive Bayes* para categorizar mensagens como *spam* ou *ham* (não spam).

---

## 📌 Equipe de Desenvolvimento
- *Alex Silva*
- *João Brandão*
- *Vinícius Porto*
- *Walber Araújo*

---

## 🛠️ Funcionalidades
✅ Treinamento do modelo a partir de arquivos CSV  
✅ Reutilização de modelos previamente treinados  
✅ Treinamento manual  
✅ Classificação de mensagens individuais  
✅ Exibição de métricas de acurácia dos modelos treinados  
✅ Interface interativa via linha de comando (CLI)  

---

## 📁 Estrutura do Projeto

```

spaML/
│── src/               # Código-fonte do projeto
│── data/              # Conjunto de dados para treinamento/teste
│── models/            # Modelos treinados salvos
│── spaML.cabal        # Arquivo de configuração do Cabal
│── README.md          # Documentação do projeto
```

---

## 📋 Requisitos
- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Cabal](https://www.haskell.org/cabal/)
- [Stack](https://docs.haskellstack.org/en/stable/README/)

---

## 🚀 Como Executar

1️⃣ Clone o repositório:
sh
git clone https://github.com/walber-araujo/spaML.git && cd spaML


2️⃣ Compile o projeto:
sh
stack build


3️⃣ Execute:

Linux/MacOS:

```sh
stack run
```

Windows:

```sh
stack run -- +RTS --io-manager=native
```

---

## 🖥️ Uso

### 📌 Opções do CLI:
| Opção | Ação |
|---------|--------|
| *1* | Treinar modelo com um arquivo CSV |
| *2* | Reutilizar modelo treinado |
| *3* | Treinamento manual |
| *4* | Classificar mensagem |
| *5* | Exibir acurácia do modelo |
| *6* | Sair |

### 📂 Formato dos arquivos CSV
Os arquivos CSV utilizados para treinamento devem seguir este formato:

label,message
ham,Hello, how are you?
spam,You won a free prize! Click here!

> Onde label pode ser *ham* ou *spam*.

---

## 📜 Licença
Este projeto está sob a [licença MIT](https://opensource.org/licenses/MIT).

---

💡 *Contribuições são bem-vindas!* Fique à vontade para abrir *issues* e enviar *pull requests*. 😊
